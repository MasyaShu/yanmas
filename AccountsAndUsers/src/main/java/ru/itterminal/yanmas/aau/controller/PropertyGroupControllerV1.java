package ru.itterminal.yanmas.aau.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.itterminal.yanmas.aau.model.PropertyGroup;
import ru.itterminal.yanmas.aau.model.dto.PropertyGroupDto;
import ru.itterminal.yanmas.aau.model.dto.PropertyGroupFilterDto;
import ru.itterminal.yanmas.aau.service.impl.PropertyGroupServiceImpl;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.commons.model.validator.scenario.Update;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;
import java.util.UUID;

@SuppressWarnings({"unchecked", "rawtypes"})
@RestController("PropertyGroupControllerV1")
@Validated
@RequestMapping("api/v1/property-group")
@RequiredArgsConstructor
public class PropertyGroupControllerV1
        extends BaseControllerImpl
        <PropertyGroup,
                PropertyGroupServiceImpl,
                PropertyGroupDto,
                PropertyGroupDto,
                PropertyGroupFilterDto> {

    private static final Class responseClazz = PropertyGroupDto.class;
    private static final Class entityClazz = PropertyGroup.class;

    @PostMapping()
    public ResponseEntity<PropertyGroupDto>
    create(@Validated(Create.class) @RequestBody PropertyGroupDto request) {
        return create(request, entityClazz, responseClazz);
    }

    @PutMapping()
    public ResponseEntity<PropertyGroupDto>
    update(@Validated(Update.class) @RequestBody PropertyGroupDto request) {
        return update(request, entityClazz, responseClazz);
    }

    @GetMapping("/{id}")
    public ResponseEntity<PropertyGroupDto> getById(@PathVariable UUID id) {
        return getById(id, responseClazz);
    }

    @GetMapping()
    public ResponseEntity<Page<PropertyGroupDto>>
    getByFilter(@Valid @RequestBody PropertyGroupFilterDto filter,
                @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
                @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        return getByFilter(filter, page, size, entityClazz, responseClazz);
    }

}
