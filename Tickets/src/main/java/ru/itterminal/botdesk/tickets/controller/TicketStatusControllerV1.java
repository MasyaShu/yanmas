package ru.itterminal.botdesk.tickets.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.model.spec.SpecificationsFactory;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.dto.TicketStatusDto;
import ru.itterminal.botdesk.tickets.model.dto.TicketStatusFilterDto;
import ru.itterminal.botdesk.tickets.service.impl.TicketStatusServiceImpl;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;
import java.security.Principal;
import java.util.UUID;

@Slf4j
@RestController("TicketStatusControllerV1")
@Validated
@RequestMapping("api/v1/ticket-status")
@RequiredArgsConstructor
public class TicketStatusControllerV1 extends BaseController {

    private final TicketStatusServiceImpl ticketStatusService;
    private final SpecificationsFactory specFactory;

    private final String ENTITY_NAME = TicketStatus.class.getSimpleName();

    @PostMapping()
    public ResponseEntity<TicketStatusDto> create(Principal principal,
                                                  @Validated(Create.class) @RequestBody TicketStatusDto request) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, request);

        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        var ticketStatus = ticketStatusService.convertRequestDtoIntoEntityWithNestedObjectsWithOnlyId(request, jwtUser.getAccountId());
        ticketStatus.setIsCanceledPredefined(false);
        ticketStatus.setIsStartedPredefined(false);
        ticketStatus.setIsFinishedPredefined(false);
        ticketStatus.setIsReopenedPredefined(false);
        TicketStatus createdTicketStatus = ticketStatusService.create(ticketStatus);
        TicketStatusDto returnedTicketStatus =
                modelMapper.map(createdTicketStatus, TicketStatusDto.class);
        log.info(CREATE_FINISH_MESSAGE, ENTITY_NAME, createdTicketStatus);
        return new ResponseEntity<>(returnedTicketStatus, HttpStatus.CREATED);
    }

    @PutMapping()
    public ResponseEntity<TicketStatusDto> update(Principal principal,
                                                  @Validated(Update.class) @RequestBody TicketStatusDto request) {
        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, request);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        TicketStatus ticketType = ticketStatusService.convertRequestDtoIntoEntityWithNestedObjectsWithOnlyId(request, jwtUser.getAccountId());
        TicketStatus updatedTicketStatus = ticketStatusService.update(ticketType);
        TicketStatusDto returnedTicketStatus = modelMapper.map(updatedTicketStatus, TicketStatusDto.class);
        log.info(UPDATE_FINISH_MESSAGE, ENTITY_NAME, updatedTicketStatus);
        return new ResponseEntity<>(returnedTicketStatus, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<Page<TicketStatusDto>> getByFilter(
            Principal user,
            @Valid @RequestBody TicketStatusFilterDto filterDto,
            @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
            @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        log.debug(FIND_INIT_MESSAGE, ENTITY_NAME, page, size, filterDto);
        var pageable = createPageable(size, page, filterDto.getSortByFields(), filterDto.getSortDirection());
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
        var accountId = jwtUser.getAccountId();
        var ticketTypesSpecification = specFactory.makeSpecificationFromEntityFilterDto(TicketStatus.class, filterDto, accountId);
        var foundTicketStatus = ticketStatusService.findAllByFilter(ticketTypesSpecification, pageable);
        var returnedTicketStatus = mapPage(foundTicketStatus, TicketStatusDto.class, pageable);
        log.debug(FIND_FINISH_MESSAGE, ENTITY_NAME, foundTicketStatus.getTotalElements());
        return new ResponseEntity<>(returnedTicketStatus, HttpStatus.OK);
    }

    @GetMapping("/{id}")
    public ResponseEntity<TicketStatusDto> getById(@PathVariable UUID id) {
        log.debug(FIND_BY_ID_INIT_MESSAGE, ENTITY_NAME, id);
        var foundTicketStatus = ticketStatusService.findByIdAndAccountId(id);
        var returnedTicketStatus = modelMapper.map(foundTicketStatus, TicketStatusDto.class);
        log.debug(FIND_BY_ID_FINISH_MESSAGE, ENTITY_NAME, foundTicketStatus);
        return new ResponseEntity<>(returnedTicketStatus, HttpStatus.OK);
    }
}
