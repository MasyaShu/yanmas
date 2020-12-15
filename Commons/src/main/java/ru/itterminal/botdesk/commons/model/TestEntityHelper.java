package ru.itterminal.botdesk.commons.model;

import java.util.List;
import java.util.UUID;

import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;

@SuppressWarnings("unused")
public interface TestEntityHelper<E extends BaseEntity, D extends BaseEntityDto, F extends BaseFilterDto> {

    // Entity
    E getRandomValidEntity();
    E getRandomInvalidEntity();
    List<E> getRandomValidEntityList(int countEntity);
    List<E> getRandomInvalidEntityList(int countEntity);
    List<E> setPredefinedValidEntityList();
    List<E> getPredefinedValidEntityList();
    E getEntityFromPredefinedValidEntityByEntityId(String entityId);

    // EntityDto
    D getRandomValidEntityDto();
    D getRandomInvalidEntityDto();

    // FilterDto
    F getRandomValidFilterDto();
    F getRandomInvalidFilterDto();
    F getPredefinedValidFilterDto();
    F getPredefinedInvalidFilterDto();

    // BaseEntity
    void setRandomValidPropertiesOfBaseEntity(E entity);
    void setPropertiesOfBaseEntity(E entity, UUID id, int version, boolean deleted, String outId);

    // BaseEntityDto
    void setRandomValidPropertiesOfBaseEntityDto(D entityDto);
    void setPropertiesOfBaseEntityDto(D entityDto, UUID id, int version, boolean deleted, String outId);

}
