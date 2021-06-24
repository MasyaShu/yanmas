package ru.itterminal.yanmas.aau.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.itterminal.yanmas.aau.model.PropertyValues;
import ru.itterminal.yanmas.aau.model.dto.PropertyValuesDtoRequest;
import ru.itterminal.yanmas.aau.model.dto.PropertyValuesDtoResponse;
import ru.itterminal.yanmas.aau.service.impl.PropertyValuesServiceImpl;
import ru.itterminal.yanmas.commons.model.dto.BaseFilterDto;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.commons.model.validator.scenario.Update;

import java.util.UUID;

@SuppressWarnings({"unchecked", "rawtypes", "unused"})
@RestController("PropertyValuesControllerV1")
@Validated
@RequestMapping("api/v1/{entityName}/{entityId}")
@RequiredArgsConstructor
public class PropertyValuesControllerV1
        extends BaseControllerImpl
        <PropertyValues,
                PropertyValuesServiceImpl,
                PropertyValuesDtoRequest,
                PropertyValuesDtoResponse,
                BaseFilterDto> {

    private static final Class responseClazz = PropertyValuesDtoResponse.class;
    private static final Class entityClazz = PropertyValues.class;

    @PostMapping("/property/{propertyId}/value")
    public ResponseEntity<PropertyValuesDtoResponse>
    create(@PathVariable String entityName, @PathVariable UUID entityId, @PathVariable UUID propertyId,
           @Validated(Create.class) @RequestBody PropertyValuesDtoRequest request) {
        request.setPropertyId(propertyId);
        request.setEntityId(entityId);
        return create(request, entityClazz, responseClazz);
    }

    @PutMapping("/property/{propertyId}/value")
    public ResponseEntity<PropertyValuesDtoResponse>
    update(@PathVariable String entityName, @PathVariable UUID entityId, @PathVariable UUID propertyId,
           @Validated(Update.class) @RequestBody PropertyValuesDtoRequest request) {
        request.setPropertyId(propertyId);
        request.setEntityId(entityId);
        return update(request, entityClazz, responseClazz);
    }

    @GetMapping("/all-properties-with-values")
    public ResponseEntity<Page<PropertyValuesDtoResponse>> getAllPropertiesWithValues
            (@PathVariable String entityName, @PathVariable UUID entityId,
             int page, int size) {
        var currentUser = getCurrentUser();
        var pageable = createPageable(size, page, null, null);
        var foundEntities = service.findAllByEntityId
                (entityId, pageable, currentUser);
        var returnedEntities = mapPage(foundEntities, PropertyValuesDtoResponse.class, pageable);
        return new ResponseEntity<>(returnedEntities, HttpStatus.OK);
    }

}
