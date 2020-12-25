package ru.itterminal.botdesk.tickets.controller;


import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.tickets.model.TicketTemplate;
import ru.itterminal.botdesk.tickets.model.dto.TicketTemplateDtoRequest;
import ru.itterminal.botdesk.tickets.model.dto.TicketTemplateDtoResponse;
import ru.itterminal.botdesk.tickets.service.impl.TicketTemplateServiceImpl;

import java.security.Principal;
import java.util.UUID;

import static java.lang.String.format;

@Slf4j
@RestController("TicketTemplateControllerV1")
@Validated
@RequestMapping("api/v1/ticketTemplate")
public class TicketTemplateControllerV1 extends BaseController {

    private final AccountServiceImpl accountService;
    private final TicketTemplateServiceImpl templateService;

    @Autowired
    public TicketTemplateControllerV1(AccountServiceImpl accountService, TicketTemplateServiceImpl templateService) {
        this.accountService = accountService;
        this.templateService = templateService;
    }

    private final String ENTITY_NAME = TicketTemplate.class.getSimpleName();

    @PostMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<TicketTemplateDtoResponse> create(Principal principal,
                                                           @Validated(Create.class) @RequestBody TicketTemplateDtoRequest request) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, request);
        TicketTemplate ticketTemplate = modelMapper.map(request, TicketTemplate.class);
        ticketTemplate.setDeleted(false);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        ticketTemplate.setAccount(accountService.findById(jwtUser.getAccountId()));
        TicketTemplate createdTicketTemplate = templateService.create(ticketTemplate);
        TicketTemplateDtoResponse returnedTicketTemplate =
                modelMapper.map(createdTicketTemplate, TicketTemplateDtoResponse.class);
        log.info(CREATE_FINISH_MESSAGE, ENTITY_NAME, request);
        return new ResponseEntity<>(returnedTicketTemplate, HttpStatus.CREATED);
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
    public ResponseEntity<TicketTemplateDtoResponse> update(Principal principal,
                                                  @Validated(Update.class) @RequestBody TicketTemplateDtoRequest request) {
        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, request);
        TicketTemplate ticketType = modelMapper.map(request, TicketTemplate.class);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        ticketType.setAccount(accountService.findById(jwtUser.getAccountId()));
        TicketTemplate updatedTicketStatus = templateService.update(ticketType);
        TicketTemplateDtoResponse returnedTicketTemplate =
                modelMapper.map(updatedTicketStatus, TicketTemplateDtoResponse.class);
        log.info(UPDATE_FINISH_MESSAGE, ENTITY_NAME, updatedTicketStatus);
        return new ResponseEntity<>(returnedTicketTemplate, HttpStatus.OK);
    }

    @PutMapping("/check-access")
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<String> updateCheckAccess() {
        String message = format(SUCCESSFUL_CHECK_ACCESS, WORD_UPDATE, ENTITY_NAME);
        log.trace(message);
        return ResponseEntity.ok(message);
    }

    @GetMapping("/{id}")
    public ResponseEntity<TicketTemplateDtoResponse> getById(Principal user, @PathVariable UUID id) {
        log.debug(FIND_BY_ID_INIT_MESSAGE, ENTITY_NAME, id);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
        TicketTemplate foundTicketTemplate;
        foundTicketTemplate = templateService.findByIdAndAccountId(id, jwtUser.getAccountId());
        TicketTemplateDtoResponse returnedTemplateStatus = modelMapper.map(foundTicketTemplate, TicketTemplateDtoResponse.class);
        log.debug(FIND_BY_ID_FINISH_MESSAGE, ENTITY_NAME, foundTicketTemplate);
        return new ResponseEntity<>(returnedTemplateStatus, HttpStatus.OK);
    }

}
