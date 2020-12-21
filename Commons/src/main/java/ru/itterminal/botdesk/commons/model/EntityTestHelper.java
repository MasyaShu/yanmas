package ru.itterminal.botdesk.commons.model;

import java.util.List;
import java.util.UUID;

import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;

@SuppressWarnings("unused")
public interface EntityTestHelper<E extends BaseEntity, DtoRequest extends BaseEntityDto,
        DtoResponse extends BaseEntityDto, F extends BaseFilterDto> {

    // Entity
    E getRandomValidEntity();
    E getRandomInvalidEntity();
    List<E> getRandomValidEntityList(int countEntity);
    List<E> getRandomInvalidEntityList(int countEntity);
    List<E> setPredefinedValidEntityList();
    List<E> getPredefinedValidEntityList();
    E getEntityFromPredefinedValidEntityByEntityId(String entityId);

    // EntityDtoRequest
    DtoRequest convertEntityToDtoRequest(E entity);

    // EntityDtoResponse
    DtoResponse convertEntityToDtoResponse(E entity);

    // BaseEntity
    void setRandomValidPropertiesOfBaseEntity(E entity);
    void setPropertiesOfBaseEntity(E entity, UUID id, int version, boolean deleted, String outId);

    // BaseEntityDto
    void setRandomValidPropertiesOfBaseEntityDto(E entityDto);
    void setPropertiesOfBaseEntityDto(E entityDto, UUID id, int version, boolean deleted, String outId);

}
