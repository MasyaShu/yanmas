package ru.itterminal.botdesk.tickets.controller;

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
import ru.itterminal.botdesk.aau.controller.BaseControllerImpl;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;
import ru.itterminal.botdesk.security.jwt.JwtUserBuilder;
import ru.itterminal.botdesk.tickets.model.SettingsAccessToTicketViaTicketTypes;
import ru.itterminal.botdesk.tickets.model.dto.SettingsAccessToTicketViaTicketTypesDtoRequest;
import ru.itterminal.botdesk.tickets.model.dto.SettingsAccessToTicketViaTicketTypesDtoResponse;
import ru.itterminal.botdesk.tickets.model.dto.SettingsAccessToTicketViaTicketTypesFilterDto;
import ru.itterminal.botdesk.tickets.service.impl.SettingsAccessToTicketViaTicketTypesServiceImpl;

@SuppressWarnings({"unchecked", "rawtypes"})
@Slf4j
@RestController
@Validated
@RequestMapping("api/v1/ticket/setting-access-via-ticket-types")
@RequiredArgsConstructor
public class SettingsAccessToTicketViaTicketTypesControllerV1
        extends BaseControllerImpl<SettingsAccessToTicketViaTicketTypes,
        SettingsAccessToTicketViaTicketTypesServiceImpl, SettingsAccessToTicketViaTicketTypesDtoRequest,
        SettingsAccessToTicketViaTicketTypesDtoResponse, SettingsAccessToTicketViaTicketTypesFilterDto> {

    private final JwtUserBuilder jwtUserBuilder;
    private static final Class responseClazz = SettingsAccessToTicketViaTicketTypesDtoResponse.class;
    private static final Class entityClazz = SettingsAccessToTicketViaTicketTypes.class;

    @PostMapping()
    public ResponseEntity<SettingsAccessToTicketViaTicketTypesDtoResponse>
    create(@Validated(Create.class) @RequestBody SettingsAccessToTicketViaTicketTypesDtoRequest request) {
        return baseCreate(request, entityClazz, responseClazz);
    }

    @PutMapping()
    public ResponseEntity<SettingsAccessToTicketViaTicketTypesDtoResponse>
    update(@Validated(Update.class) @RequestBody SettingsAccessToTicketViaTicketTypesDtoRequest request) {
        return baseUpdate(request, entityClazz, responseClazz);
    }

    @GetMapping("/{id}")
    public ResponseEntity<SettingsAccessToTicketViaTicketTypesDtoResponse> getById(@PathVariable UUID id) {
        return baseGetById(id, entityClazz, responseClazz);
    }

    @GetMapping()
    public ResponseEntity<Page<SettingsAccessToTicketViaTicketTypesDtoResponse>>
    getByFilter(@Valid @RequestBody SettingsAccessToTicketViaTicketTypesFilterDto filter,
                @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
                @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        jwtUserBuilder.throwAccessDeniedExceptionIfCurrentUserFromOuterGroup();
        return baseGetByFilter(filter, page, size, entityClazz, responseClazz);
    }
}
