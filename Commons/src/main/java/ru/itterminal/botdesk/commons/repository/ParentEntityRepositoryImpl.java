package ru.itterminal.botdesk.commons.repository;

import static java.lang.String.format;

import java.util.UUID;

import javax.persistence.EntityManager;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.jpa.repository.support.JpaEntityInformation;
import org.springframework.data.jpa.repository.support.SimpleJpaRepository;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.exception.NullEntityException;
import ru.itterminal.botdesk.commons.model.BaseEntity;

/**
 * Implements common methods for all entities (extending BaseEntity) such as: markDeleted() & unmarkDeleted().
 */

@Slf4j
public class ParentEntityRepositoryImpl<T extends BaseEntity>
    extends SimpleJpaRepository<T, UUID>
    implements ParentEntityRepository<T> {

    private final EntityManager entityManager;

    private static final String ENTITY_NOT_EXIST_MESSAGE = "%s with id:'%s' not exist";
    private static final String ENTITY_NULL_MESSAGE = "Entity is null";

    @Autowired
    public ParentEntityRepositoryImpl(JpaEntityInformation<T, ?> entityInformation, EntityManager entityManager) {
        super(entityInformation, entityManager);
        this.entityManager = entityManager;
    }

    @Override
    public T create(T entity) {
        entityManager.persist(entity);
        entityManager.flush();
        return entity;
    }

    @SuppressWarnings("OptionalGetWithoutIsPresent")
    @Override
    public T update(T entity) {
        isExist(entity);
        entity = entityManager.merge(entity);
        entityManager.flush();
        entityManager.detach(entity);
        return findById(entity.getId()).get();
    }

    private void isExist(T entity) {
        if (entity == null) {
            log.error(ENTITY_NULL_MESSAGE);
            throw new NullEntityException(ENTITY_NULL_MESSAGE);
        }
        if (findById(entity.getId()).isEmpty()) {
            String message = format(ENTITY_NOT_EXIST_MESSAGE, entity.getClass().getSimpleName(), entity.getId());
            log.error(message);
            throw new EntityNotExistException(message);
        }
    }
}
