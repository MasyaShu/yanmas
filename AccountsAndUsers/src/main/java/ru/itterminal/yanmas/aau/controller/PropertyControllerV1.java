package ru.itterminal.yanmas.aau.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.itterminal.yanmas.aau.model.Property;
import ru.itterminal.yanmas.aau.model.EntityName;
import ru.itterminal.yanmas.aau.model.dto.PropertyDto;
import ru.itterminal.yanmas.aau.model.dto.PropertyFilterDto;
import ru.itterminal.yanmas.aau.service.impl.PropertyServiceImpl;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.commons.model.validator.scenario.Update;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;
import java.util.UUID;

@SuppressWarnings({"unchecked", "rawtypes"})
@RestController("PropertyControllerV1")
@Validated
@RequestMapping("api/v1/{entityName}/property")
@RequiredArgsConstructor
public class PropertyControllerV1
        extends BaseControllerImpl<
        Property,
        PropertyServiceImpl,
        PropertyDto,
        PropertyDto,
        PropertyFilterDto> {

    private static final Class responseClazz = PropertyDto.class;
    private static final Class entityClazz = Property.class;

    @PostMapping()
    public ResponseEntity<PropertyDto>
    create(@PathVariable EntityName entityName, @Validated(Create.class) @RequestBody PropertyDto request) {
        request.setEntityName(entityName.toString());
        return create(request, entityClazz, responseClazz);
    }

    @PutMapping()
    public ResponseEntity<PropertyDto>
    update(@PathVariable EntityName entityName, @Validated(Update.class) @RequestBody PropertyDto request) {
        request.setEntityName(entityName.toString());
        return update(request, entityClazz, responseClazz);
    }

    @SuppressWarnings("unused")
    @GetMapping("/{id}")
    public ResponseEntity<PropertyDto> getById(@PathVariable EntityName entityName, @PathVariable UUID id) {
        return getById(id, responseClazz);
    }

    @GetMapping()
    public ResponseEntity<Page<PropertyDto>>
    getByFilter(@Valid @RequestBody PropertyFilterDto filter,
                @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
                @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        return getByFilter(filter, page, size, entityClazz, responseClazz);
    }

}
