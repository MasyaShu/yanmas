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
import ru.itterminal.botdesk.tickets.model.TicketTypes;
import ru.itterminal.botdesk.tickets.model.dto.TicketTypesDto;
import ru.itterminal.botdesk.tickets.model.dto.TicketTypesFilterDto;
import ru.itterminal.botdesk.tickets.model.spec.TicketTypesSpec;
import ru.itterminal.botdesk.tickets.service.impl.TicketTypesServiceImpl;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;
import java.security.Principal;
import java.util.UUID;

import static java.lang.String.format;


@Slf4j
@RestController("TicketTypesControllerV1")
@Validated
@RequestMapping("api/v1/ticketTypes")
public class TicketTypesControllerV1 extends BaseController {

    private final TicketTypesServiceImpl service;
    private final TicketTypesSpec spec;
    private final AccountServiceImpl accountService;

    @Autowired
    public TicketTypesControllerV1(TicketTypesServiceImpl service, TicketTypesSpec typesSpec, AccountServiceImpl accountService) {
        this.spec = typesSpec;
        this.service = service;
        this.accountService = accountService;
    }

    private final String ENTITY_NAME = TicketTypes.class.getSimpleName();

    @PostMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<TicketTypesDto> create(Principal principal,
                                                 @Validated(Create.class) @RequestBody TicketTypesDto request) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, request);
        TicketTypes ticketTypes = modelMapper.map(request, TicketTypes.class);
        ticketTypes.setDeleted(false);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        ticketTypes.setAccount(accountService.findById(jwtUser.getAccountId()));
        TicketTypes createdTicketTypes = service.create(ticketTypes);
        TicketTypesDto returnedTicketTypes =
                modelMapper.map(createdTicketTypes, TicketTypesDto.class);
        log.info(CREATE_FINISH_MESSAGE, ENTITY_NAME, createdTicketTypes);
        return new ResponseEntity<>(returnedTicketTypes, HttpStatus.CREATED);
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
    public ResponseEntity<TicketTypesDto> update(Principal principal,
            @Validated(Update.class) @RequestBody TicketTypesDto request) {
        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, request);
        TicketTypes ticketTypes = modelMapper.map(request, TicketTypes.class);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        ticketTypes.setAccount(accountService.findById(jwtUser.getAccountId()));
        TicketTypes updatedTicketTypes = service.update(ticketTypes);
        TicketTypesDto returnedTicketTypes =
                modelMapper.map(updatedTicketTypes, TicketTypesDto.class);
        log.info(UPDATE_FINISH_MESSAGE, ENTITY_NAME, updatedTicketTypes);
        return new ResponseEntity<>(returnedTicketTypes, HttpStatus.OK);
    }

    @PutMapping("/check-access")
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<String> updateCheckAccess() {
        String message = format(SUCCESSFUL_CHECK_ACCESS, WORD_UPDATE, ENTITY_NAME);
        log.trace(message);
        return ResponseEntity.ok(message);
    }

    @SuppressWarnings("DuplicatedCode")
    @GetMapping()
    public ResponseEntity<Page<TicketTypesDto>> getByFilter(
            Principal user,
            @Valid @RequestBody TicketTypesFilterDto filter,
            @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
            @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        log.debug(FIND_INIT_MESSAGE, ENTITY_NAME, page, size, filter);
        if (filter.getDirection() == null) {
            filter.setDirection("ASC");
        }
        if (filter.getSortBy() == null) {
            filter.setSortBy("name");
        }
        if (filter.getDeleted() == null) {
            filter.setDeleted("all");
        }
        Pageable pageable =
                PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(filter.getDirection()),
                        filter.getSortBy()));
        Page<TicketTypes> foundTicketTypes;
        Page<TicketTypesDto> returnedTicketTypes;
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
        Specification<TicketTypes> ticketTypesSpecification = Specification
                .where(filter.getName() == null ? null : spec.getTicketTypesByNameSpec(filter.getName()))
                .and(filter.getComment() == null ? null : spec.getTicketTypesByCommentSpec(filter.getComment()))
                .and(spec.getEntityByDeletedSpec(BaseFilterDto.FilterByDeleted.fromString(filter.getDeleted())))
                .and(spec.getEntityByAccountSpec(jwtUser.getAccountId()))
                .and(filter.getOutId() == null ? null :  spec.getEntityByOutIdSpec(filter.getOutId()));

        foundTicketTypes = service.findAllByFilter(ticketTypesSpecification, pageable);
        returnedTicketTypes = mapPage(foundTicketTypes, TicketTypesDto.class, pageable);
        log.debug(FIND_FINISH_MESSAGE, ENTITY_NAME, foundTicketTypes.getTotalElements());
        return new ResponseEntity<>(returnedTicketTypes, HttpStatus.OK);
    }
    @GetMapping("/{id}")
    public ResponseEntity<TicketTypesDto> getById(Principal user, @PathVariable UUID id) {
        log.debug(FIND_BY_ID_INIT_MESSAGE, ENTITY_NAME, id);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
        TicketTypes foundTicketTypes;
        foundTicketTypes = service.findByIdAndAccountId(id, jwtUser.getAccountId());
        TicketTypesDto returnedTicketTypes = modelMapper.map(foundTicketTypes, TicketTypesDto.class);
        log.debug(FIND_BY_ID_FINISH_MESSAGE, ENTITY_NAME, foundTicketTypes);
        return new ResponseEntity<>(returnedTicketTypes, HttpStatus.OK);
    }

    @DeleteMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<Void> physicalDelete() {
        throw new UnsupportedOperationException("Physical delete will be implement in the further");
    }

}
