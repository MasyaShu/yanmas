package ru.itterminal.yanmas.tickets.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import ru.itterminal.yanmas.commons.controller.BaseController;

@Slf4j
@RestController("TicketEventControllerV1")
@Validated
@RequestMapping("api/v1/ticket/event")
@RequiredArgsConstructor
public class TicketEventControllerV1 extends BaseController {

}
