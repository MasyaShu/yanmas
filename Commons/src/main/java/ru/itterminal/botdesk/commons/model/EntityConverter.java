package ru.itterminal.botdesk.commons.model;

import java.util.UUID;

import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;

public interface EntityConverter <E extends BaseEntity, R extends BaseEntityDto>{

    E convertRequestDtoIntoEntityWithNestedObjectsWithOnlyId(R request, UUID accountId);

    default void setBaseEntityPropertiesFromRequestDtoIntoEntity(
            R request,
            E entity) {
        if (request.getId() != null) {
            entity.setId(request.getId());
        }
        if (request.getOutId() != null) {
            entity.setOutId(request.getOutId());
        }
        if (request.getDeleted() != null) {
            entity.setDeleted(request.getDeleted());
        }
        if (request.getVersion() != null) {
            //noinspection deprecation
            entity.setVersion(request.getVersion());
        }
    }
}
