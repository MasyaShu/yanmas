package ru.itterminal.botdesk.tickets.controller;


import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.dto.TicketStatusDto;
import ru.itterminal.botdesk.tickets.model.dto.TicketStatusFilterDto;
import ru.itterminal.botdesk.tickets.model.spec.TicketStatusSpec;
import ru.itterminal.botdesk.tickets.service.impl.TicketStatusServiceImpl;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;
import java.security.Principal;
import java.util.UUID;

import static java.lang.String.format;

@Slf4j
@RestController("TicketStatusControllerV1")
@Validated
@RequestMapping("api/v1/ticket-status")
public class TicketStatusControllerV1 extends BaseController {

    private final TicketStatusServiceImpl service;
    private final TicketStatusSpec spec;
    private final AccountServiceImpl accountService;

    @Autowired
    public TicketStatusControllerV1(TicketStatusServiceImpl service, TicketStatusSpec statusSpec, AccountServiceImpl accountService) {
        this.spec = statusSpec;
        this.service = service;
        this.accountService = accountService;
    }

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
            @Valid @RequestBody TicketStatusFilterDto filter,
            @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
            @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        log.debug(FIND_INIT_MESSAGE, ENTITY_NAME, page, size, filter);
        if (filter.getDirection() == null) {
            filter.setDirection("ASC");
        }
        if (filter.getDeleted() == null) {
            filter.setDeleted("all");
        }
        Pageable pageable =
                PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(filter.getDirection()),
                        "sortIndex"));
        Page<TicketStatus> foundTicketStatus;
        Page<TicketStatusDto> returnedTicketStatus;
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
        Specification<TicketStatus> ticketTypesSpecification = Specification
                .where(filter.getName() == null ? null : spec.getTicketStatusByNameSpec(filter.getName()))
                .and(spec.getEntityByDeletedSpec(BaseFilterDto.FilterByDeleted.fromString(filter.getDeleted())))
                .and(spec.getEntityByAccountSpec(jwtUser.getAccountId()))
                .and(filter.getOutId() == null ? null :  spec.getEntityByOutIdSpec(filter.getOutId()));

        foundTicketStatus = service.findAllByFilter(ticketTypesSpecification, pageable);
        returnedTicketStatus = mapPage(foundTicketStatus, TicketStatusDto.class, pageable);
        log.debug(FIND_FINISH_MESSAGE, ENTITY_NAME, foundTicketStatus.getTotalElements());
        return new ResponseEntity<>(returnedTicketStatus, HttpStatus.OK);
    }
    @GetMapping("/{id}")
    public ResponseEntity<TicketStatusDto> getById(Principal user, @PathVariable UUID id) {
        log.debug(FIND_BY_ID_INIT_MESSAGE, ENTITY_NAME, id);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
        TicketStatus foundTicketStatus;
        foundTicketStatus = service.findByIdAndAccountId(id, jwtUser.getAccountId());
        TicketStatusDto returnedTicketStatus = modelMapper.map(foundTicketStatus, TicketStatusDto.class);
        log.debug(FIND_BY_ID_FINISH_MESSAGE, ENTITY_NAME, foundTicketStatus);
        return new ResponseEntity<>(returnedTicketStatus, HttpStatus.OK);
    }
}
