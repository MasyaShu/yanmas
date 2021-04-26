package ru.itterminal.yanmas.tickets.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.itterminal.yanmas.aau.controller.BaseControllerImpl;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;
import ru.itterminal.yanmas.tickets.model.TicketEvent;
import ru.itterminal.yanmas.tickets.model.dto.*;
import ru.itterminal.yanmas.tickets.service.impl.TicketEventServiceImpl;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;
import java.util.UUID;

@SuppressWarnings({"unchecked", "rawtypes"})
@Slf4j
@RestController("TicketEventControllerV1")
@Validated
@RequestMapping("api/v1/ticket/event")
@RequiredArgsConstructor
public class TicketEventControllerV1 extends BaseControllerImpl<TicketEvent,
        TicketEventServiceImpl, TicketEventDtoRequest,
        TicketEventDtoResponse, TicketEventFilterDto> {
    private final JwtUserBuilder jwtUserBuilder;
    private static final Class responseClazz = TicketEventDtoResponse.class;
    private static final Class entityClazz = TicketEvent.class;

    @PostMapping()
    public ResponseEntity<TicketEventDtoResponse>
    create(@Validated(Create.class) @RequestBody TicketEventDtoRequest request) {
        return baseCreate(request, entityClazz, responseClazz);
    }

    @GetMapping("/{id}")
    public ResponseEntity<TicketEventDtoResponse> getById(@PathVariable UUID id) {
        return baseGetById(id, entityClazz, responseClazz);
    }

    @GetMapping()
    public ResponseEntity<Page<TicketEventDtoResponse>>
    getByFilter(@Valid @RequestBody TicketEventFilterDto filter,
                @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
                @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        jwtUserBuilder.throwAccessDeniedExceptionIfCurrentUserFromOuterGroup();
        return baseGetByFilter(filter, page, size, entityClazz, responseClazz);
    }
}
