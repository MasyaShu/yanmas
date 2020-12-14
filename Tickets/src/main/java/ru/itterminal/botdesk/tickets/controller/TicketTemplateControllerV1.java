package ru.itterminal.botdesk.tickets.controller;


import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.TicketTemplate;
import ru.itterminal.botdesk.tickets.model.dto.TicketStatusDto;
import ru.itterminal.botdesk.tickets.model.dto.TicketTemplateDto;

import java.security.Principal;

@Slf4j
@RestController("TicketTemplateControllerV1")
@Validated
@RequestMapping("api/v1/ticketTemplate")
public class TicketTemplateControllerV1 extends BaseController {

    private final AccountServiceImpl accountService;

    @Autowired
    public TicketTemplateControllerV1(AccountServiceImpl accountService) {
        this.accountService = accountService;
    }

    private final String ENTITY_NAME = TicketTemplate.class.getSimpleName();


    @PostMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<TicketTemplateDto> create(Principal principal,
                                                  @Validated(Create.class) @RequestBody TicketTemplateDto request) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, request);
        TicketTemplate ticketTemplate = modelMapper.map(request, TicketTemplate.class);
        ticketTemplate.setDeleted(false);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        ticketTemplate.setAccount(accountService.findById(jwtUser.getAccountId()));
        //TicketStatus createdTicketStatus = service.create(ticketType);
        TicketTemplateDto returnedTicketTemplate =
                modelMapper.map(request, TicketTemplateDto.class);
        log.info(CREATE_FINISH_MESSAGE, ENTITY_NAME, request);
        return new ResponseEntity<>(returnedTicketTemplate, HttpStatus.CREATED);
    }
}
