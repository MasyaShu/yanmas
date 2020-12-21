package ru.itterminal.botdesk.commons.model;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.UUID;

import org.modelmapper.ModelMapper;

import com.github.javafaker.Faker;

import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;

@SuppressWarnings("deprecation")
public abstract class EntityTestHelperImpl<E extends BaseEntity, DtoRequest extends BaseEntityDto,
        DtoResponse extends BaseEntityDto, F extends BaseFilterDto>
        implements EntityTestHelper<E, DtoRequest, DtoResponse, F> {

    protected Faker fakerRU = new Faker(new Locale("ru", "RU"));
    protected Faker fakerEN = new Faker(new Locale("en", "US"));
    protected ModelMapper modelMapper = new ModelMapper();

    @SuppressWarnings("FieldMayBeFinal")
    private List<E> predefinedValidEntityList = new ArrayList<>();

    // Entity

    @Override
    public final List<E> getRandomValidEntityList(int countEntity) {
        List<E> list = new ArrayList<>();
        for (int i = 0; i < countEntity; i++) {
            list.add(getRandomValidEntity());
        }
        return list;
    }

    @Override
    public final List<E> getRandomInvalidEntityList(int countEntity) {
        List<E> list = new ArrayList<>();
        for (int i = 0; i < countEntity; i++) {
            list.add(getRandomInvalidEntity());
        }
        return list;
    }

    @Override
    public final List<E> getPredefinedValidEntityList() {
        if (predefinedValidEntityList.isEmpty()) {
            predefinedValidEntityList.addAll(setPredefinedValidEntityList());
        }
        return predefinedValidEntityList;
    }

    @Override
    public final E getEntityFromPredefinedValidEntityByEntityId(String entityId) {
        List<E> listEntities = getPredefinedValidEntityList();
        UUID entityUUID = UUID.fromString(entityId);
        for (E entity : listEntities) {
            if (entity.getId().equals(entityUUID)) {
                return entity;
            }
        }
        return null;
    }

    // EntityDto

    @Override
    public DtoRequest convertEntityToDtoRequest(E entity) {
        return null;
    }

    @Override
    public DtoResponse convertEntityToDtoResponse(E entity) {
        return null;
    }

    // BaseEntity

    @Override
    public final void setRandomValidPropertiesOfBaseEntity(E entity) {
        entity.setId(UUID.randomUUID());
        entity.setVersion(fakerRU.number().numberBetween(0, 100));
        entity.setDeleted(1 == fakerRU.number().numberBetween(0, 2));
        entity.setOutId(UUID.randomUUID().toString());
        entity.generateDisplayName();
    }

    @Override
    public final void setPropertiesOfBaseEntity(E entity, UUID id, int version, boolean deleted, String outId) {
        entity.setId(id);
        entity.setVersion(version);
        entity.setDeleted(deleted);
        entity.setOutId(outId);
        entity.generateDisplayName();
    }

    // BaseEntityDto

    @Override
    public final void setRandomValidPropertiesOfBaseEntityDto(E entityDto) {
        entityDto.setId(UUID.randomUUID());
        entityDto.setVersion(fakerRU.number().numberBetween(0, 100));
        entityDto.setDeleted(1 == fakerRU.number().numberBetween(0, 2));
        entityDto.setOutId(UUID.randomUUID().toString());
    }

    @Override
    public final void setPropertiesOfBaseEntityDto(E entityDto, UUID id, int version, boolean deleted,
                                                   String outId) {
        entityDto.setId(id);
        entityDto.setVersion(version);
        entityDto.setDeleted(deleted);
        entityDto.setOutId(outId);
    }
}
