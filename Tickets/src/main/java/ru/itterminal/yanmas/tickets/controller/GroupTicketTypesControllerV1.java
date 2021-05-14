package ru.itterminal.yanmas.tickets.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.itterminal.yanmas.aau.controller.BaseControllerImpl;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.commons.model.validator.scenario.Update;
import ru.itterminal.yanmas.tickets.model.GroupTicketTypes;
import ru.itterminal.yanmas.tickets.model.dto.GroupTicketTypesDtoRequest;
import ru.itterminal.yanmas.tickets.model.dto.GroupTicketTypesDtoResponse;
import ru.itterminal.yanmas.tickets.model.dto.GroupTicketTypesFilterDto;
import ru.itterminal.yanmas.tickets.service.impl.GroupTicketTypesServiceImpl;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;
import java.util.UUID;

@SuppressWarnings({"unchecked", "rawtypes"})
@RestController("GroupTicketTypesControllerV1")
@Validated
@RequestMapping("api/v1/ticket/type/group")
@RequiredArgsConstructor
public class GroupTicketTypesControllerV1 extends BaseControllerImpl
        <GroupTicketTypes,
                GroupTicketTypesServiceImpl,
                GroupTicketTypesDtoRequest,
                GroupTicketTypesDtoResponse,
                GroupTicketTypesFilterDto> {

    private static final Class responseClazz = GroupTicketTypesDtoResponse.class;
    private static final Class entityClazz = GroupTicketTypes.class;

    @PostMapping()
    public ResponseEntity<GroupTicketTypesDtoResponse>
    create(@Validated(Create.class) @RequestBody GroupTicketTypesDtoRequest request) {
        return create(request, entityClazz, responseClazz);
    }

    @PutMapping()
    public ResponseEntity<GroupTicketTypesDtoResponse>
    update(@Validated(Update.class) @RequestBody GroupTicketTypesDtoRequest request) {
        return update(request, entityClazz, responseClazz);
    }

    @GetMapping()
    public ResponseEntity<Page<GroupTicketTypesDtoResponse>>
    getByFilter(@Valid @RequestBody GroupTicketTypesFilterDto filter,
                @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
                @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        return getByFilter(filter, page, size, entityClazz, responseClazz);
    }

    @GetMapping("/{id}")
    public ResponseEntity<GroupTicketTypesDtoResponse> getById(@PathVariable UUID id) {
        return getById(id, responseClazz);
    }
}
