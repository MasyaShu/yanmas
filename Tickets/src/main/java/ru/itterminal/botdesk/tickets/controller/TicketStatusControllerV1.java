package ru.itterminal.botdesk.tickets.controller;

import static java.lang.String.format;

import java.security.Principal;
import java.util.UUID;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;

import org.springframework.data.domain.Page;
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
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.model.spec.SpecificationsFactory;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.dto.TicketStatusDto;
import ru.itterminal.botdesk.tickets.model.dto.TicketStatusFilterDto;
import ru.itterminal.botdesk.tickets.service.impl.TicketStatusServiceImpl;

@Slf4j
@RestController("TicketStatusControllerV1")
@Validated
@RequestMapping("api/v1/ticket-status")
@RequiredArgsConstructor
public class TicketStatusControllerV1 extends BaseController {

    private final TicketStatusServiceImpl service;
    private final AccountServiceImpl accountService;
    private final SpecificationsFactory specFactory;

    private final String ENTITY_NAME = TicketStatus.class.getSimpleName();

    @PostMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<TicketStatusDto> create(Principal principal,
                                                  @Validated(Create.class) @RequestBody TicketStatusDto request) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, request);
        TicketStatus ticketType = modelMapper.map(request, TicketStatus.class);
        ticketType.setDeleted(false);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        ticketType.setAccount(accountService.findById(jwtUser.getAccountId()));
        ticketType.setIsCanceledPredefined(false);
        ticketType.setIsStartedPredefined(false);
        ticketType.setIsFinishedPredefined(false);
        ticketType.setIsReopenedPredefined(false);
        TicketStatus createdTicketStatus = service.create(ticketType);
        TicketStatusDto returnedTicketStatus =
                modelMapper.map(createdTicketStatus, TicketStatusDto.class);
        log.info(CREATE_FINISH_MESSAGE, ENTITY_NAME, createdTicketStatus);
        return new ResponseEntity<>(returnedTicketStatus, HttpStatus.CREATED);
    }

    @PostMapping("/check-access")
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<String> createCheckAccess() {
        String message = format(SUCCESSFUL_CHECK_ACCESS, WORD_CREATE, ENTITY_NAME);
        log.trace(message);
        return ResponseEntity.ok(message);
    }

    @PutMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<TicketStatusDto> update(Principal principal,
                                                  @Validated(Update.class) @RequestBody TicketStatusDto request) {
        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, request);
        TicketStatus ticketType = modelMapper.map(request, TicketStatus.class);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        ticketType.setAccount(accountService.findById(jwtUser.getAccountId()));
        TicketStatus updatedTicketStatus = service.update(ticketType);
        TicketStatusDto returnedTicketStatus =
                modelMapper.map(updatedTicketStatus, TicketStatusDto.class);
        log.info(UPDATE_FINISH_MESSAGE, ENTITY_NAME, updatedTicketStatus);
        return new ResponseEntity<>(returnedTicketStatus, HttpStatus.OK);
    }

    @PutMapping("/check-access")
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<String> updateCheckAccess() {
        String message = format(SUCCESSFUL_CHECK_ACCESS, WORD_UPDATE, ENTITY_NAME);
        log.trace(message);
        return ResponseEntity.ok(message);
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
        var foundTicketStatus = service.findAllByFilter(ticketTypesSpecification, pageable);
        var returnedTicketStatus = mapPage(foundTicketStatus, TicketStatusDto.class, pageable);
        log.debug(FIND_FINISH_MESSAGE, ENTITY_NAME, foundTicketStatus.getTotalElements());
        return new ResponseEntity<>(returnedTicketStatus, HttpStatus.OK);
    }

    @GetMapping("/{id}")
    public ResponseEntity<TicketStatusDto> getById(@PathVariable UUID id) {
        log.debug(FIND_BY_ID_INIT_MESSAGE, ENTITY_NAME, id);
        var foundTicketStatus = service.findByIdAndAccountId(id);
        var returnedTicketStatus = modelMapper.map(foundTicketStatus, TicketStatusDto.class);
        log.debug(FIND_BY_ID_FINISH_MESSAGE, ENTITY_NAME, foundTicketStatus);
        return new ResponseEntity<>(returnedTicketStatus, HttpStatus.OK);
    }
}
