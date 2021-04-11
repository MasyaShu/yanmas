package ru.itterminal.yanmas.tickets.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.itterminal.yanmas.commons.controller.BaseController;
import ru.itterminal.yanmas.commons.model.validator.scenario.Update;
import ru.itterminal.yanmas.security.jwt.JwtUser;
import ru.itterminal.yanmas.tickets.model.TicketCounter;
import ru.itterminal.yanmas.tickets.model.dto.TicketCounterDto;
import ru.itterminal.yanmas.tickets.service.impl.TicketCounterServiceImpl;

import java.security.Principal;

@Slf4j
@RestController("TicketCounterControllerV1")
@Validated
@RequestMapping("api/v1/ticket/counter")
@RequiredArgsConstructor
public class TicketCounterControllerV1 extends BaseController {

    private final TicketCounterServiceImpl service;
    private final String ENTITY_NAME = TicketCounter.class.getSimpleName();

    @PutMapping()
    public ResponseEntity<TicketCounterDto> update(Principal principal,
                                                   @Validated(Update.class) @RequestBody TicketCounterDto request) {
        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, request);
        var ticketCounter = modelMapper.map(request, TicketCounter.class);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        ticketCounter.setId(jwtUser.getAccountId());
        var updatedTicketCounter = service.update(ticketCounter);
        var returnedTicketCounter = modelMapper.map(updatedTicketCounter, TicketCounterDto.class);
        log.info(UPDATE_FINISH_MESSAGE, ENTITY_NAME, request);
        return new ResponseEntity<>(returnedTicketCounter, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<TicketCounterDto> getByAccountId(Principal user) {
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
        log.debug(FIND_BY_ID_INIT_MESSAGE, ENTITY_NAME, jwtUser.getAccountId());
        var foundTicketCounter = service.getTicketCounter(jwtUser.getAccountId());
        var returnedTicketCounter = modelMapper.map(foundTicketCounter, TicketCounterDto.class);
        log.debug(FIND_BY_ID_FINISH_MESSAGE, ENTITY_NAME, foundTicketCounter);
        return new ResponseEntity<>(returnedTicketCounter, HttpStatus.OK);
    }

}
