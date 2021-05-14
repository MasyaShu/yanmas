package ru.itterminal.yanmas.tickets.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.itterminal.yanmas.aau.controller.BaseControllerImpl;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.commons.model.validator.scenario.Update;
import ru.itterminal.yanmas.tickets.model.TicketTemplate;
import ru.itterminal.yanmas.tickets.model.dto.TicketTemplateDtoRequest;
import ru.itterminal.yanmas.tickets.model.dto.TicketTemplateDtoResponse;
import ru.itterminal.yanmas.tickets.model.dto.TicketTemplateFilterDto;
import ru.itterminal.yanmas.tickets.service.impl.TicketTemplateServiceImpl;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;
import java.util.UUID;

@SuppressWarnings({"DuplicatedCode", "unchecked", "rawtypes"})
@RestController("TicketTemplateControllerV1")
@RequestMapping("api/v1/ticket/template")
@RequiredArgsConstructor
public class TicketTemplateControllerV1 extends BaseControllerImpl<
        TicketTemplate,
        TicketTemplateServiceImpl,
        TicketTemplateDtoRequest,
        TicketTemplateDtoResponse,
        TicketTemplateFilterDto> {

    private static final Class responseClazz = TicketTemplateDtoResponse.class;
    private static final Class entityClazz = TicketTemplate.class;

    @PostMapping()
    public ResponseEntity<TicketTemplateDtoResponse>
    create(@Validated(Create.class) @RequestBody TicketTemplateDtoRequest request) {
        return create(request, entityClazz, responseClazz);
    }

    @PutMapping()
    public ResponseEntity<TicketTemplateDtoResponse>
    update(@Validated(Update.class) @RequestBody TicketTemplateDtoRequest request) {
        return update(request, entityClazz, responseClazz);
    }

    @GetMapping()
    public ResponseEntity<Page<TicketTemplateDtoResponse>>
    getByFilter(@Valid @RequestBody TicketTemplateFilterDto filter,
                @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
                @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        return getByFilter(filter, page, size, entityClazz, responseClazz);
    }

    @GetMapping("/{id}")
    public ResponseEntity<TicketTemplateDtoResponse> getById(@PathVariable UUID id) {
        return getById(id, responseClazz);
    }
}
