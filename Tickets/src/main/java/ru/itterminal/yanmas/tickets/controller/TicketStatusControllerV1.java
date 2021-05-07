package ru.itterminal.yanmas.tickets.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.itterminal.yanmas.aau.controller.BaseControllerImpl;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.commons.model.validator.scenario.Update;
import ru.itterminal.yanmas.tickets.model.TicketStatus;
import ru.itterminal.yanmas.tickets.model.dto.TicketStatusDto;
import ru.itterminal.yanmas.tickets.model.dto.TicketStatusFilterDto;
import ru.itterminal.yanmas.tickets.service.impl.TicketStatusServiceImpl;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;
import java.util.UUID;

@SuppressWarnings({"unchecked", "rawtypes"})
@RestController("TicketStatusControllerV1")
@Validated
@RequestMapping("api/v1/ticket/status")
@RequiredArgsConstructor
public class TicketStatusControllerV1
        extends BaseControllerImpl
        <TicketStatus,
                TicketStatusServiceImpl,
                TicketStatusDto,
                TicketStatusDto,
                TicketStatusFilterDto> {

    private static final Class responseClazz = TicketStatusDto.class;
    private static final Class entityClazz = TicketStatus.class;

    @PostMapping()
    public ResponseEntity<TicketStatusDto>
    create(@Validated(Create.class) @RequestBody TicketStatusDto request) {
        return create(request, entityClazz, responseClazz);
    }

    @PutMapping()
    public ResponseEntity<TicketStatusDto>
    update(@Validated(Update.class) @RequestBody TicketStatusDto request) {
        return update(request, entityClazz, responseClazz);
    }

    @GetMapping()
    public ResponseEntity<Page<TicketStatusDto>>
    getByFilter(@Valid @RequestBody TicketStatusFilterDto filter,
                @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
                @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        return getByFilter(filter, page, size, entityClazz, responseClazz);
    }

    @GetMapping("/{id}")
    public ResponseEntity<TicketStatusDto> getById(@PathVariable UUID id) {
        return getById(id, responseClazz);
    }

}

