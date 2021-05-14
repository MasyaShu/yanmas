package ru.itterminal.yanmas.tickets.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.itterminal.yanmas.aau.controller.BaseControllerImpl;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.commons.model.validator.scenario.Update;
import ru.itterminal.yanmas.tickets.model.TicketType;
import ru.itterminal.yanmas.tickets.model.dto.TicketTypeDto;
import ru.itterminal.yanmas.tickets.model.dto.TicketTypeFilterDto;
import ru.itterminal.yanmas.tickets.service.impl.TicketTypeServiceImpl;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;
import java.util.UUID;

@SuppressWarnings({"unchecked", "rawtypes"})
@RestController("TicketTypeControllerV1")
@Validated
@RequestMapping("api/v1/ticket/type")
@RequiredArgsConstructor
public class TicketTypeControllerV1 extends BaseControllerImpl
        <TicketType,
                TicketTypeServiceImpl,
                TicketTypeDto,
                TicketTypeDto,
                TicketTypeFilterDto> {

    private static final Class responseClazz = TicketTypeDto.class;
    private static final Class entityClazz = TicketType.class;

    @PostMapping()
    public ResponseEntity<TicketTypeDto>
    create(@Validated(Create.class) @RequestBody TicketTypeDto request) {
        return create(request, entityClazz, responseClazz);
    }

    @PutMapping()
    public ResponseEntity<TicketTypeDto>
    update(@Validated(Update.class) @RequestBody TicketTypeDto request) {
        return update(request, entityClazz, responseClazz);
    }

    @GetMapping()
    public ResponseEntity<Page<TicketTypeDto>>
    getByFilter(@Valid @RequestBody TicketTypeFilterDto filter,
                @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
                @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        return getByFilter(filter, page, size, entityClazz, responseClazz);
    }

    @GetMapping("/{id}")
    public ResponseEntity<TicketTypeDto> getById(@PathVariable UUID id) {
        return getById(id, responseClazz);
    }
}
