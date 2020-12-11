package ru.itterminal.botdesk.commons.model;

import java.util.List;
import java.util.Locale;
import java.util.UUID;

import com.github.javafaker.Faker;

import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;

@SuppressWarnings({"unused", "deprecation"})
public interface TestEntityHelper<E extends BaseEntity, D extends BaseEntityDto, F extends BaseFilterDto> {

    // Entity
    E getRandomValidEntity();

    E getRandomInvalidEntity();

    List<E> getRandomValidEntityList(int countEntity);

    List<E> getRandomInvalidEntityList(int countEntity);

    List<E> getPredefinedValidEntityList();

    List<E> getPredefinedInvalidEntityList();

    // EntityDto
    D getRandomValidEntityDto();

    D getRandomInvalidEntityDto();

    Faker fakerRU = new Faker(new Locale("ru", "RU"));
    Faker fakerEN = new Faker(new Locale("en", "US"));

    default void setRandomValidPropertiesOfBaseEntity(E entity) {
        entity.setId(UUID.randomUUID());
        entity.setVersion(fakerRU.number().numberBetween(0, 100));
        entity.setDeleted(1 == fakerRU.number().numberBetween(0, 2));
        entity.setOutId(UUID.randomUUID().toString());
    }

    default void setRandomValidPropertiesOfBaseEntityDto(D entityDto) {
        entityDto.setId(UUID.randomUUID());
        entityDto.setVersion(fakerRU.number().numberBetween(0, 100));
        entityDto.setDeleted(1 == fakerRU.number().numberBetween(0, 2));
        entityDto.setOutId(UUID.randomUUID().toString());
    }

    default void setPropertiesOfBaseEntity(E entity, UUID id, int version, boolean deleted, String outId) {
        entity.setId(id);
        entity.setVersion(version);
        entity.setDeleted(deleted);
        entity.setOutId(outId);
    }

    default void setPropertiesOfBaseEntityDto(D entityDto, UUID id, int version, boolean deleted, String outId) {
        entityDto.setId(id);
        entityDto.setVersion(version);
        entityDto.setDeleted(deleted);
        entityDto.setOutId(outId);
    }

    // FilterDto
    default F getRandomValidFilterDto() {
        return null;
    }

    default F getRandomInvalidFilterDto() {
        return null;
    }

    default F getPredefinedValidFilterDto() {
        return null;
    }

    default F getPredefinedInvalidFilterDto() {
        return null;
    }
}
