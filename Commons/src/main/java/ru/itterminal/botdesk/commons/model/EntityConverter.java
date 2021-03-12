package ru.itterminal.botdesk.commons.model;

import java.util.UUID;

import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;

public interface EntityConverter <E extends BaseEntity, R extends BaseEntityDto>{

    E convertRequestDtoIntoEntityWithNestedObjectsWithOnlyId(R request, UUID accountId);
}
