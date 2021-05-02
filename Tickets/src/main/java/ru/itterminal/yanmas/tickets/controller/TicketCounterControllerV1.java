package ru.itterminal.yanmas.tickets.controller;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.commons.controller.BaseController;
import ru.itterminal.yanmas.commons.model.validator.scenario.Update;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;
import ru.itterminal.yanmas.tickets.model.TicketCounter;
import ru.itterminal.yanmas.tickets.model.dto.TicketCounterDto;
import ru.itterminal.yanmas.tickets.service.impl.TicketCounterServiceImpl;

@RestController("TicketCounterControllerV1")
@Validated
@RequestMapping("api/v1/ticket/counter")
@RequiredArgsConstructor
public class TicketCounterControllerV1 extends BaseController {

    private final TicketCounterServiceImpl service;
    private final JwtUserBuilder jwtUserBuilder;

    @PutMapping()
    public ResponseEntity<TicketCounterDto> update(@Validated(Update.class) @RequestBody TicketCounterDto request) {
        var ticketCounter = modelMapper.map(request, TicketCounter.class);
        var jwtUser = jwtUserBuilder.getJwtUser();
        ticketCounter.setId(jwtUser.getAccountId());
        var updatedTicketCounter = service.update(ticketCounter);
        var returnedTicketCounter = modelMapper.map(updatedTicketCounter, TicketCounterDto.class);
        return new ResponseEntity<>(returnedTicketCounter, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<TicketCounterDto> getByAccountId() {
        var jwtUser = jwtUserBuilder.getJwtUser();
        var foundTicketCounter = service.getTicketCounter(jwtUser.getAccountId());
        var returnedTicketCounter = modelMapper.map(foundTicketCounter, TicketCounterDto.class);
        return new ResponseEntity<>(returnedTicketCounter, HttpStatus.OK);
    }

}
