package ru.itterminal.yanmas.tickets.controller;

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
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.yanmas.aau.controller.BaseControllerImpl;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.commons.model.validator.scenario.Update;
import ru.itterminal.yanmas.tickets.model.SettingsAccessToTicketTypes;
import ru.itterminal.yanmas.tickets.model.dto.SettingsAccessToTicketTypesDtoRequest;
import ru.itterminal.yanmas.tickets.model.dto.SettingsAccessToTicketTypesDtoResponse;
import ru.itterminal.yanmas.tickets.model.dto.SettingsAccessToTicketTypesFilterDto;
import ru.itterminal.yanmas.tickets.service.business_handler.SettingsAccessToTicketTypesBusinessHandler;
import ru.itterminal.yanmas.tickets.service.impl.SettingsAccessToTicketTypesServiceImpl;

@SuppressWarnings({"unchecked", "rawtypes"})
@Slf4j
@RestController
@Validated
@RequestMapping("api/v1/ticket/type/setting-access")
@RequiredArgsConstructor
public class SettingsAccessToTicketTypesControllerV1
        extends BaseControllerImpl<
        SettingsAccessToTicketTypes,
        SettingsAccessToTicketTypesBusinessHandler,
        SettingsAccessToTicketTypesServiceImpl,
        SettingsAccessToTicketTypesDtoRequest,
        SettingsAccessToTicketTypesDtoResponse,
        SettingsAccessToTicketTypesFilterDto> {

    private static final Class responseClazz = SettingsAccessToTicketTypesDtoResponse.class;
    private static final Class entityClazz = SettingsAccessToTicketTypes.class;

    @PostMapping()
    public ResponseEntity<SettingsAccessToTicketTypesDtoResponse>
    create(@Validated(Create.class) @RequestBody SettingsAccessToTicketTypesDtoRequest request) {
        return baseCreate(request, entityClazz, responseClazz);
    }

    @PutMapping()
    public ResponseEntity<SettingsAccessToTicketTypesDtoResponse>
    update(@Validated(Update.class) @RequestBody SettingsAccessToTicketTypesDtoRequest request) {
        return baseUpdate(request, entityClazz, responseClazz);
    }

    @GetMapping("/{id}")
    public ResponseEntity<SettingsAccessToTicketTypesDtoResponse> getById(@PathVariable UUID id) {
        return baseGetById(id, responseClazz);
    }

    @GetMapping()
    public ResponseEntity<Page<SettingsAccessToTicketTypesDtoResponse>>
    getByFilter(@Valid @RequestBody SettingsAccessToTicketTypesFilterDto filter,
                @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
                @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        return baseGetByFilter(filter, page, size, entityClazz, responseClazz);
    }
}
