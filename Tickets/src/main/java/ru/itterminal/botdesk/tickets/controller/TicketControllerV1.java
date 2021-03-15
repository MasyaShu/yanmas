package ru.itterminal.botdesk.tickets.controller;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.EXIST_IN;
import static ru.itterminal.botdesk.commons.model.filter.ListOfBaseEntityFilter.TypeComparisonForListOfBaseEntityFilter.CONTAINS_ALL_OF_LIST;

import java.security.Principal;
import java.util.List;
import java.util.UUID;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;

import org.springframework.data.domain.Page;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.model.filter.BaseEntityFilter;
import ru.itterminal.botdesk.commons.model.filter.ListOfBaseEntityFilter;
import ru.itterminal.botdesk.commons.model.spec.SpecificationsFactory;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.tickets.model.Ticket;
import ru.itterminal.botdesk.tickets.model.dto.TicketDtoRequest;
import ru.itterminal.botdesk.tickets.model.dto.TicketDtoResponse;
import ru.itterminal.botdesk.tickets.model.dto.TicketFilterDto;
import ru.itterminal.botdesk.tickets.service.impl.TicketServiceImpl;

@Slf4j
@RestController("TicketControllerV1")
@Validated
@RequestMapping("api/v1/ticket")
@RequiredArgsConstructor
public class TicketControllerV1 extends BaseController {
    public static final String GROUP = "group";
    public static final String AUTHOR = "author";
    public static final String OBSERVERS = "observers";

    private final TicketServiceImpl ticketService;
    private final UserServiceImpl userService;
    private final SpecificationsFactory specFactory;

    private final String ENTITY_NAME = Ticket.class.getSimpleName();

    @PostMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR', 'AUTHOR')")
    public ResponseEntity<TicketDtoResponse> create
            (Principal principal, @Validated(Create.class) @RequestBody TicketDtoRequest ticketDtoRequest) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, ticketDtoRequest);
        var jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        var currentUser = userService.findByEmail(jwtUser.getUsername());
        var createdTicket = ticketService.create(
                ticketService.convertRequestDtoIntoEntityWithNestedObjectsWithOnlyId(
                        ticketDtoRequest,
                        jwtUser.getAccountId()
                ),
                currentUser
        );
        var returnedTicket = modelMapper.map(createdTicket, TicketDtoResponse.class);
        log.info(CREATE_FINISH_MESSAGE, ENTITY_NAME, createdTicket);
        return new ResponseEntity<>(returnedTicket, HttpStatus.CREATED);
    }

    @PostMapping("/check-access")
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR', 'AUTHOR')")
    public ResponseEntity<String> createCheckAccess() {
        String message = format(SUCCESSFUL_CHECK_ACCESS, WORD_CREATE, ENTITY_NAME);
        log.trace(message);
        return ResponseEntity.ok(message);
    }

    @PutMapping("/check-access")
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR', 'AUTHOR')")
    public ResponseEntity<String> updateCheckAccess() {
        String message = format(SUCCESSFUL_CHECK_ACCESS, WORD_UPDATE, ENTITY_NAME);
        log.trace(message);
        return ResponseEntity.ok(message);
    }

    @GetMapping()
    public ResponseEntity<Page<TicketDtoResponse>> getByFilter(
            Principal user,
            @Valid @RequestBody TicketFilterDto filterDto,
            @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
            @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        log.debug(FIND_INIT_MESSAGE, ENTITY_NAME, page, size, filterDto);
        var pageable = createPageable(size, page, filterDto.getSortByFields(), filterDto.getSortDirection());
        var jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
        var accountId = jwtUser.getAccountId();
        var ticketSpecification = specFactory.makeSpecificationFromEntityFilterDto(Ticket.class, filterDto, accountId);
        setAdditionalConditionsIntoSpecAccordingPermissionOfCurrentUser(jwtUser, ticketSpecification);
        var foundTickets = ticketService.findAllByFilter(ticketSpecification, pageable);
        var returnedTicketStatus = mapPage(foundTickets, TicketDtoResponse.class, pageable);
        log.debug(FIND_FINISH_MESSAGE, ENTITY_NAME, foundTickets.getTotalElements());
        return new ResponseEntity<>(returnedTicketStatus, HttpStatus.OK);
    }

    @GetMapping("/{id}")
    public ResponseEntity<TicketDtoResponse> getById(@PathVariable UUID id) {
        log.debug(FIND_BY_ID_INIT_MESSAGE, ENTITY_NAME, id);
        var foundTicket = ticketService.findByIdAndAccountId(id);
        var returnedTicket = modelMapper.map(foundTicket, TicketDtoResponse.class);
        log.debug(FIND_BY_ID_FINISH_MESSAGE, ENTITY_NAME, foundTicket);
        return new ResponseEntity<>(returnedTicket, HttpStatus.OK);
    }

    private void setAdditionalConditionsIntoSpecAccordingPermissionOfCurrentUser
            (JwtUser jwtUser, Specification<Ticket> spec) {

        var currentUser = userService.findByEmail(jwtUser.getUsername());
        var isCurrentUserFromInnerGroup = currentUser.getGroup().getIsInner();
        var nameOfRoleOfCurrentUser = currentUser.getRole().getName();

        if ((nameOfRoleOfCurrentUser.equals(Roles.ADMIN.toString())
                || nameOfRoleOfCurrentUser.equals(Roles.EXECUTOR.toString())
        ) && Boolean.FALSE.equals(isCurrentUserFromInnerGroup)) {
            var filterByGroupOfCurrentUser = BaseEntityFilter.builder()
                    .typeComparison(EXIST_IN.toString())
                    .listOfIdEntities(List.of(currentUser.getGroup().getId()))
                    .build();
            Specification<Ticket> additionConditionByGroupOfCurrentUser =
                    specFactory.makeSpecification(Ticket.class, GROUP, filterByGroupOfCurrentUser);
            spec = spec.and(additionConditionByGroupOfCurrentUser);
        }

        if (nameOfRoleOfCurrentUser.equals(Roles.AUTHOR.toString())) {
            var filterByAuthorOfTicket = BaseEntityFilter.builder()
                    .typeComparison(EXIST_IN.toString())
                    .listOfIdEntities(List.of(currentUser.getId()))
                    .build();
            Specification<Ticket> additionConditionByAuthorOfTicket =
                    specFactory.makeSpecification(Ticket.class, AUTHOR, filterByAuthorOfTicket);
            var filterByListOfObservers = ListOfBaseEntityFilter.builder()
                    .typeComparison(CONTAINS_ALL_OF_LIST.toString())
                    .listOfIdEntities(List.of(currentUser.getId()))
                    .build();
            Specification<Ticket> additionConditionByObserversOfTicket =
                    specFactory.makeSpecification(Ticket.class, OBSERVERS, filterByListOfObservers);
            spec = spec.and(additionConditionByAuthorOfTicket.or(additionConditionByObserversOfTicket));
        }

        if (nameOfRoleOfCurrentUser.equals(Roles.OBSERVER.toString())) {
            var filterByListOfObservers = ListOfBaseEntityFilter.builder()
                    .typeComparison(CONTAINS_ALL_OF_LIST.toString())
                    .listOfIdEntities(List.of(currentUser.getId()))
                    .build();
            Specification<Ticket> additionConditionByObserversOfTicket =
                    specFactory.makeSpecification(Ticket.class, OBSERVERS, filterByListOfObservers);
            //noinspection UnusedAssignment
            spec = spec.and(additionConditionByObserversOfTicket);
        }
    }
}
