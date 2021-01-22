package ru.itterminal.botdesk.tickets.controller;

import static java.lang.String.format;

import java.security.Principal;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.tickets.model.TicketCounter;
import ru.itterminal.botdesk.tickets.model.dto.TicketCounterDto;
import ru.itterminal.botdesk.tickets.service.impl.TicketCounterServiceImpl;

@Slf4j
@RestController("TicketCounterControllerV1")
@Validated
@RequestMapping("api/v1/ticket-counter")
@RequiredArgsConstructor
public class TicketCounterControllerV1 extends BaseController {

    private final TicketCounterServiceImpl service;

    private final String ENTITY_NAME = TicketCounter.class.getSimpleName();

    @PutMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<TicketCounterDto>
    update(Principal principal, @Validated(Update.class) @RequestBody TicketCounterDto request) {
        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, request);
        var ticketCounter = modelMapper.map(request, TicketCounter.class);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        ticketCounter.setId(jwtUser.getAccountId());
        var updatedTicketCounter = service.update(ticketCounter);
        var returnedTicketCounter = modelMapper.map(updatedTicketCounter, TicketCounterDto.class);
        log.info(UPDATE_FINISH_MESSAGE, ENTITY_NAME, request);
        return new ResponseEntity<>(returnedTicketCounter, HttpStatus.OK);
    }

    @PutMapping("/check-access")
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<String> updateCheckAccess() {
        String message = format(SUCCESSFUL_CHECK_ACCESS, WORD_UPDATE, ENTITY_NAME);
        log.trace(message);
        return ResponseEntity.ok(message);
    }

    @GetMapping()
    public ResponseEntity<TicketCounterDto> getByAccountId(Principal user) {
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
        log.debug(FIND_BY_ID_INIT_MESSAGE, ENTITY_NAME, jwtUser.getAccountId());
        var foundTicketCounter = service.findById(jwtUser.getAccountId());
        var returnedTicketCounter  = modelMapper.map(foundTicketCounter, TicketCounterDto.class);
        log.debug(FIND_BY_ID_FINISH_MESSAGE, ENTITY_NAME, foundTicketCounter);
        return new ResponseEntity<>(returnedTicketCounter, HttpStatus.OK);
    }

}
