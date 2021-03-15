package ru.itterminal.botdesk.tickets.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.model.spec.SpecificationsFactory;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.tickets.model.TicketTemplate;
import ru.itterminal.botdesk.tickets.model.dto.TicketTemplateDtoRequest;
import ru.itterminal.botdesk.tickets.model.dto.TicketTemplateDtoResponse;
import ru.itterminal.botdesk.tickets.model.dto.TicketTemplateFilterDto;
import ru.itterminal.botdesk.tickets.service.impl.TicketTemplateServiceImpl;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;
import java.security.Principal;
import java.util.UUID;

import static java.lang.String.format;

@Slf4j
@RestController("TicketTemplateControllerV1")
@RequestMapping("api/v1/ticket-template")
@RequiredArgsConstructor
public class TicketTemplateControllerV1 extends BaseController {

    public static final String ACCESS_IS_DENIED = "Access is denied";
    private final TicketTemplateServiceImpl templateService;
    private final SpecificationsFactory specFactory;

    private final String ENTITY_NAME = TicketTemplate.class.getSimpleName();

    @PostMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR')")
    public ResponseEntity<TicketTemplateDtoResponse> create(Principal principal,
                                                            @Validated(Create.class) @RequestBody TicketTemplateDtoRequest request) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, request);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        var ticketTemplate = templateService.convertRequestDtoIntoEntityWithNestedObjectsWithOnlyId(request, jwtUser.getAccountId());
        TicketTemplate createdTicketTemplate = templateService.create(ticketTemplate);
        TicketTemplateDtoResponse returnedTicketTemplate =
                modelMapper.map(createdTicketTemplate, TicketTemplateDtoResponse.class);
        log.info(CREATE_FINISH_MESSAGE, ENTITY_NAME, request);
        return new ResponseEntity<>(returnedTicketTemplate, HttpStatus.CREATED);
    }

    @PostMapping("/check-access")
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR')")
    public ResponseEntity<String> createCheckAccess() {
        String message = format(SUCCESSFUL_CHECK_ACCESS, WORD_CREATE, ENTITY_NAME);
        log.trace(message);
        return ResponseEntity.ok(message);
    }

    @PutMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR')")
    public ResponseEntity<TicketTemplateDtoResponse> update(Principal principal,
                                                            @Validated(Update.class) @RequestBody TicketTemplateDtoRequest request) {
        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, request);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        var ticketTemplate = templateService.convertRequestDtoIntoEntityWithNestedObjectsWithOnlyId(request, jwtUser.getAccountId());
        TicketTemplate updatedTicketStatus = templateService.update(ticketTemplate);
        TicketTemplateDtoResponse returnedTicketTemplate =
                modelMapper.map(updatedTicketStatus, TicketTemplateDtoResponse.class);
        log.info(UPDATE_FINISH_MESSAGE, ENTITY_NAME, updatedTicketStatus);
        return new ResponseEntity<>(returnedTicketTemplate, HttpStatus.OK);
    }

    @PutMapping("/check-access")
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR')")
    public ResponseEntity<String> updateCheckAccess() {
        String message = format(SUCCESSFUL_CHECK_ACCESS, WORD_UPDATE, ENTITY_NAME);
        log.trace(message);
        return ResponseEntity.ok(message);
    }

    @GetMapping("/{id}")
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR')")
    public ResponseEntity<TicketTemplateDtoResponse> getById(@PathVariable UUID id) {
        JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (!jwtUser.isInnerGroup()) {
            throw new AccessDeniedException(ACCESS_IS_DENIED);
        }
        log.debug(FIND_BY_ID_INIT_MESSAGE, ENTITY_NAME, id);
        var foundTicketTemplate = templateService.findByIdAndAccountId(id);
        var returnedTemplateStatus = modelMapper.map(foundTicketTemplate, TicketTemplateDtoResponse.class);
        log.debug(FIND_BY_ID_FINISH_MESSAGE, ENTITY_NAME, foundTicketTemplate);
        return new ResponseEntity<>(returnedTemplateStatus, HttpStatus.OK);
    }

    @GetMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR')")
    public ResponseEntity<Page<TicketTemplateDtoResponse>> getByFilter(
            Principal user,
            @Valid @RequestBody TicketTemplateFilterDto filterDto,
            @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
            @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
        if (!jwtUser.isInnerGroup()) {
            throw new AccessDeniedException(ACCESS_IS_DENIED);
        }
        log.debug(FIND_INIT_MESSAGE, ENTITY_NAME, page, size, filterDto);
        var pageable = createPageable(size, page, filterDto.getSortByFields(), filterDto.getSortDirection());
        var accountId = jwtUser.getAccountId();
        var ticketTemplateSpecification = specFactory.makeSpecificationFromEntityFilterDto(TicketTemplate.class, filterDto, accountId);
        var foundTicketTemplate = templateService.findAllByFilter(ticketTemplateSpecification, pageable);
        var returnedTicketTemplate = mapPage(foundTicketTemplate, TicketTemplateDtoResponse.class, pageable);
        log.debug(FIND_FINISH_MESSAGE, ENTITY_NAME, foundTicketTemplate.getTotalElements());
        return new ResponseEntity<>(returnedTicketTemplate, HttpStatus.OK);
    }
}
