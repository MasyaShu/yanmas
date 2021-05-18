package ru.itterminal.yanmas.tickets.controller;

import java.util.List;
import java.util.UUID;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;

import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.controller.BaseControllerImpl;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.WhoWatchedEntityServiceImpl;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.commons.model.validator.scenario.Update;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.model.dto.TicketDtoRequest;
import ru.itterminal.yanmas.tickets.model.dto.TicketDtoResponse;
import ru.itterminal.yanmas.tickets.model.dto.TicketFilterDto;
import ru.itterminal.yanmas.tickets.service.impl.TicketServiceImpl;

@SuppressWarnings({"unchecked", "rawtypes"})
@RestController("TicketControllerV1")
@Validated
@RequestMapping("api/v1/ticket")
@RequiredArgsConstructor
public class TicketControllerV1 extends BaseControllerImpl<
        Ticket,
        TicketServiceImpl,
        TicketDtoRequest,
        TicketDtoResponse,
        TicketFilterDto> {

    private static final Class responseClazz = TicketDtoResponse.class;
    private static final Class entityClazz = Ticket.class;

    private final UserServiceImpl userService;
    private final WhoWatchedEntityServiceImpl whoWatchedEntityService;

    @PostMapping()
    public ResponseEntity<TicketDtoResponse> create(@Validated(Create.class) @RequestBody TicketDtoRequest request) {
        ResponseEntity<TicketDtoResponse> createdTicket = create(request, entityClazz, responseClazz);
        @SuppressWarnings("ConstantConditions")
        var createdTicketId = createdTicket.getBody().getId();
        whoWatchedEntityService.watched(List.of(createdTicketId), userService.getCurrentUserFromJwtUser());
        return createdTicket;
    }

    @PutMapping()
    public ResponseEntity<TicketDtoResponse> update(@Validated(Update.class) @RequestBody TicketDtoRequest request) {
        var updatedTicket = update(request, entityClazz, responseClazz);
        whoWatchedEntityService.watched(List.of(request.getId()), userService.getCurrentUserFromJwtUser());
        return updatedTicket;
    }

    @GetMapping("/{id}")
    public ResponseEntity<TicketDtoResponse> getById(@PathVariable UUID id) {
        var foundTicket = getById(id, responseClazz);
        whoWatchedEntityService.watched(List.of(id), userService.getCurrentUserFromJwtUser());
        return foundTicket;
    }

    @GetMapping()
    public ResponseEntity<Page<TicketDtoResponse>> getByFilter(
            @Valid @RequestBody TicketFilterDto filter,
            @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
            @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        return getByFilter(filter, page, size, entityClazz, responseClazz);
    }
}
