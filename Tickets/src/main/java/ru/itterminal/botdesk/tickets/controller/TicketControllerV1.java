package ru.itterminal.botdesk.tickets.controller;

import static java.lang.String.format;

import java.security.Principal;
import java.util.UUID;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.model.spec.SpecificationsFactory;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;
import ru.itterminal.botdesk.files.service.FileServiceImpl;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.tickets.model.Ticket;
import ru.itterminal.botdesk.tickets.model.dto.TicketDtoRequest;
import ru.itterminal.botdesk.tickets.model.dto.TicketDtoResponse;
import ru.itterminal.botdesk.tickets.service.impl.TicketServiceImpl;
import ru.itterminal.botdesk.tickets.service.impl.TicketSettingServiceImpl;
import ru.itterminal.botdesk.tickets.service.impl.TicketStatusServiceImpl;
import ru.itterminal.botdesk.tickets.service.impl.TicketTemplateServiceImpl;
import ru.itterminal.botdesk.tickets.service.impl.TicketTypeServiceImpl;

@Slf4j
@RestController("TicketControllerV1")
@Validated
@RequestMapping("api/v1/ticket")
@RequiredArgsConstructor
public class TicketControllerV1 extends BaseController {

    private final TicketServiceImpl service;
    private final AccountServiceImpl accountService;
    private final UserServiceImpl userService;
    private final TicketTemplateServiceImpl ticketTemplateService;
    private final TicketTypeServiceImpl ticketTypeService;
    private final TicketStatusServiceImpl ticketStatusService;
    private final FileServiceImpl fileService;
    private final TicketSettingServiceImpl ticketSettingService;
    @SuppressWarnings("unused")
    private final SpecificationsFactory specFactory;

    private final String ENTITY_NAME = Ticket.class.getSimpleName();

    @PostMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR', 'AUTHOR')")
    public ResponseEntity<TicketDtoResponse> create
            (Principal principal, @Validated(Create.class) @RequestBody TicketDtoRequest ticketDtoRequest) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, ticketDtoRequest);
        var jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        var accountId = jwtUser.getAccountId();
        var currentUser = userService.findByEmail(jwtUser.getUsername()).get();
        var ticket = modelMapper.map(ticketDtoRequest, Ticket.class);
        setNestedObjectsIntoEntityFromEntityDtoRequest(ticket, ticketDtoRequest, accountId);
        var createdTicket = service.create(ticket, currentUser);
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

    @PutMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR', 'AUTHOR')")
    public ResponseEntity<TicketDtoResponse> update
            (Principal principal, @Validated(Update.class) @RequestBody TicketDtoRequest ticketDtoRequest) {
        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, ticketDtoRequest);
        var ticket = modelMapper.map(ticketDtoRequest, Ticket.class);
        var jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        var accountId = jwtUser.getAccountId();
        var currentUser = userService.findByEmail(jwtUser.getUsername()).get();
        setNestedObjectsIntoEntityFromEntityDtoRequest(ticket, ticketDtoRequest, accountId);
        var updatedTicket = service.update(ticket, currentUser);
        var returnedTicketStatus = modelMapper.map(updatedTicket, TicketDtoResponse.class);
        log.info(UPDATE_FINISH_MESSAGE, ENTITY_NAME, updatedTicket);
        return new ResponseEntity<>(returnedTicketStatus, HttpStatus.OK);
    }

    @PutMapping("/check-access")
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR', 'AUTHOR')")
    public ResponseEntity<String> updateCheckAccess() {
        String message = format(SUCCESSFUL_CHECK_ACCESS, WORD_UPDATE, ENTITY_NAME);
        log.trace(message);
        return ResponseEntity.ok(message);
    }

    //    @GetMapping()
    //    public ResponseEntity<Page<TicketStatusDto>> getByFilter(
    //            Principal user,
    //            @Valid @RequestBody TicketStatusFilterDto filterDto,
    //            @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
    //            @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
    //        log.debug(FIND_INIT_MESSAGE, ENTITY_NAME, page, size, filterDto);
    //        var pageable = createPageable(size, page, filterDto.getSortByFields(), filterDto.getSortDirection());
    //        Page<TicketStatus> foundTicketStatus;
    //        Page<TicketStatusDto> returnedTicketStatus;
    //        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
    //        var accountId = jwtUser.getAccountId();
    //        var ticketTypesSpecification = specFactory.makeSpecificationFromEntityFilterDto(TicketStatus.class, filterDto, accountId);
    //        foundTicketStatus = service.findAllByFilter(ticketTypesSpecification, pageable);
    //        returnedTicketStatus = mapPage(foundTicketStatus, TicketStatusDto.class, pageable);
    //        log.debug(FIND_FINISH_MESSAGE, ENTITY_NAME, foundTicketStatus.getTotalElements());
    //        return new ResponseEntity<>(returnedTicketStatus, HttpStatus.OK);
    //    }
    //
    //    @GetMapping("/{id}")
    //    public ResponseEntity<TicketStatusDto> getById(Principal user, @PathVariable UUID id) {
    //        log.debug(FIND_BY_ID_INIT_MESSAGE, ENTITY_NAME, id);
    //        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
    //        TicketStatus foundTicketStatus;
    //        foundTicketStatus = service.findByIdAndAccountId(id, jwtUser.getAccountId());
    //        TicketStatusDto returnedTicketStatus = modelMapper.map(foundTicketStatus, TicketStatusDto.class);
    //        log.debug(FIND_BY_ID_FINISH_MESSAGE, ENTITY_NAME, foundTicketStatus);
    //        return new ResponseEntity<>(returnedTicketStatus, HttpStatus.OK);
    //    }

    private void setNestedObjectsIntoEntityFromEntityDtoRequest(Ticket ticket, TicketDtoRequest request,
                                                                UUID accountId) {

        ticket.setAccount(accountService.findById(accountId));

        ticket.setAuthor(userService.findByIdAndAccountId(request.getAuthor(), accountId));

        ticket.setGroup(ticket.getAuthor().getGroup());

        var valuesForTicketPredefinedOrFromSettings =
                ticketSettingService.getSettingOrPredefinedValuesForTicket(
                        accountId,
                        ticket.getGroup().getId(),
                        ticket.getAuthor().getId()
                );

        if (request.getTicketType() != null) {
            var ticketTypeId = request.getTicketType();
            ticket.setTicketType(ticketTypeService.findByIdAndAccountId(ticketTypeId, accountId));
        } else {
            ticket.setTicketType(valuesForTicketPredefinedOrFromSettings.getTicketTypeForNew());
        }

        if (request.getTicketStatus() != null) {
            var ticketStatusId = request.getTicketStatus();
            ticket.setTicketStatus(ticketStatusService.findByIdAndAccountId(ticketStatusId, accountId));
        } else {
            ticket.setTicketStatus(valuesForTicketPredefinedOrFromSettings.getTicketStatusForNew());
        }

        if (request.getTicketTemplate() != null) {
            var ticketTemplateId = request.getTicketTemplate();
            ticket.setTicketTemplate(ticketTemplateService.findByIdAndAccountId(ticketTemplateId, accountId));
        }

        ticket.setObservers(userService.findAllByAccountIdAndListId(accountId, request.getObservers()));
        ticket.setExecutors(userService.findAllByAccountIdAndListId(accountId, request.getExecutors()));
        ticket.setFiles(fileService.findAllByAccountIdAndListId(accountId, request.getFiles()));

    }
}
