package ru.itterminal.yanmas.aau.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.dto.GroupDto;
import ru.itterminal.yanmas.aau.model.dto.GroupFilterDto;
import ru.itterminal.yanmas.aau.service.business_handler.impl.GroupBusinessHandlerImpl;
import ru.itterminal.yanmas.aau.service.impl.GroupServiceImpl;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.commons.model.validator.scenario.Update;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;
import java.util.UUID;

@SuppressWarnings({"unchecked", "rawtypes"})
@RestController("GroupControllerV1")
@Validated
@RequestMapping("api/v1/user/group")
@RequiredArgsConstructor
public class GroupControllerV1
        extends BaseControllerImpl<
        Group,
        GroupBusinessHandlerImpl,
        GroupServiceImpl,
        GroupDto,
        GroupDto,
        GroupFilterDto> {

    private static final Class responseClazz = GroupDto.class;
    private static final Class entityClazz = Group.class;

    @PostMapping()
    public ResponseEntity<GroupDto>
    create(@Validated(Create.class) @RequestBody GroupDto request) {
        return baseCreate(request, entityClazz, responseClazz);
    }

    @PutMapping()
    public ResponseEntity<GroupDto>
    update(@Validated(Update.class) @RequestBody GroupDto request) {
        return baseUpdate(request, entityClazz, responseClazz);
    }

    @GetMapping("/{id}")
    public ResponseEntity<GroupDto> getById(@PathVariable UUID id) {
        return baseGetById(id, responseClazz);
    }

    @GetMapping()
    public ResponseEntity<Page<GroupDto>>
    getByFilter(@Valid @RequestBody GroupFilterDto filter,
                @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
                @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        return baseGetByFilter(filter, page, size, entityClazz, responseClazz);
    }

}
