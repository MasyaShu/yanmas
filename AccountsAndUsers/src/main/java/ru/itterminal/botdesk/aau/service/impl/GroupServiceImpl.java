package ru.itterminal.botdesk.aau.service.impl;

import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.projection.GroupUniqueFields;
import ru.itterminal.botdesk.aau.repository.GroupRepository;
import ru.itterminal.botdesk.aau.service.validator.GroupOperationValidator;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceImpl;

import javax.persistence.OptimisticLockException;
import java.util.List;
import java.util.UUID;

import static java.lang.String.format;

@Slf4j
@Service
@Transactional
public class GroupServiceImpl extends CrudServiceImpl<Group, GroupOperationValidator, GroupRepository> {

    public static final String NOT_FOUND_GROUPS_BY_UNIQUE_FIELDS_USER_IS_NULL =
            "Not found groups by unique fields, group is null";
    public static final String NOT_FOUND_GROUPS_BY_UNIQUE_FIELDS_ACCOUNT_IS_NULL =
            "Not found groups by unique fields, account is null";
    public static final String NOT_FOUND_GROUPS_BY_UNIQUE_FIELDS_NAME_IS_NULL =
            "Not found groups by unique fields, name is null";
    public static final String NOT_FOUND_GROUPS_BY_UNIQUE_FIELDS_ID_IS_NULL =
            "Not found groups by unique fields, id is null";
    public static final String START_FIND_USER_BY_UNIQUE_FIELDS =
            "Start find user by unique fields, name: {} and not id: {} and not account: {}";
    public static final String NOT_FOUND_USER_BY_ID_AND_ACCOUNT_ID = "Not found group by id: %s and accountId: %s";
    public static final String START_FIND_GROUP_BY_ID_AND_ACCOUNT_ID = "Start find group by id: {} and accountId: {}";


    @Override
    public Group create(Group entity) {
        entity.setDeleted(false);
        entity.setIsDeprecated(false);
        return super.create(entity);
    }

    @Override
    public Group update(Group entity) {
        validator.beforeUpdate(entity);
        log.trace(format(UPDATE_INIT_MESSAGE, entity.getClass().getSimpleName(), entity.getId(), entity));
        Group entityFromDatabase = repository.findById(entity.getId()).orElseThrow(() -> {
            String message = format(ENTITY_NOT_EXIST_MESSAGE, entity.getClass().getSimpleName(), entity.getId());
            log.error(message);
            return new EntityNotExistException(message);
        });
        try {
            entity.setIsInner(entityFromDatabase.getIsInner());
            Group updatedEntity = repository.update(entity);
            log.trace(format(UPDATE_FINISH_MESSAGE, entity.getClass().getSimpleName(), entity.getId(), updatedEntity));
            return updatedEntity;
        }
        catch (OptimisticLockException ex) {
            throw new OptimisticLockingFailureException(format(VERSION_INVALID_MESSAGE, entity.getId()));
        }
    }

    public List<GroupUniqueFields> findByUniqueFields(Group group) {
        if (group == null) {
            log.error(NOT_FOUND_GROUPS_BY_UNIQUE_FIELDS_USER_IS_NULL);
            throw new EntityNotExistException(NOT_FOUND_GROUPS_BY_UNIQUE_FIELDS_USER_IS_NULL);
        }
        if (group.getName() == null) {
            log.error(NOT_FOUND_GROUPS_BY_UNIQUE_FIELDS_NAME_IS_NULL);
            throw new EntityNotExistException(NOT_FOUND_GROUPS_BY_UNIQUE_FIELDS_NAME_IS_NULL);
        }
        if (group.getId() == null) {
            log.error(NOT_FOUND_GROUPS_BY_UNIQUE_FIELDS_ID_IS_NULL);
            throw new EntityNotExistException(NOT_FOUND_GROUPS_BY_UNIQUE_FIELDS_ID_IS_NULL);
        }
        if (group.getAccount() == null) {
            log.error(NOT_FOUND_GROUPS_BY_UNIQUE_FIELDS_ACCOUNT_IS_NULL);
            throw new EntityNotExistException(NOT_FOUND_GROUPS_BY_UNIQUE_FIELDS_ACCOUNT_IS_NULL);
        }
        log.trace(START_FIND_USER_BY_UNIQUE_FIELDS, group.getName(), group.getId(), group.getAccount());
        return repository.getByNameAndAccount_IdAndIdNot(group.getName(), group.getAccount().getId(),  group.getId());
    }

    @Transactional(readOnly = true)
    public Group findByIdAndAccountId(UUID id, UUID accountId) {
        if (id == null) {
            log.error(format(NOT_FOUND_USER_BY_ID_AND_ACCOUNT_ID, id, accountId));
            throw new EntityNotExistException(format(NOT_FOUND_USER_BY_ID_AND_ACCOUNT_ID, id, accountId));
        }
        if (accountId == null) {
            log.error(format(NOT_FOUND_USER_BY_ID_AND_ACCOUNT_ID, id, accountId));
            throw new EntityNotExistException(format(NOT_FOUND_USER_BY_ID_AND_ACCOUNT_ID, id, accountId));
        }
        log.trace(START_FIND_GROUP_BY_ID_AND_ACCOUNT_ID, id, accountId);
        return repository.getByIdAndAccount_Id(id, accountId).orElseThrow(
                () -> new EntityNotExistException(format(NOT_FOUND_USER_BY_ID_AND_ACCOUNT_ID, id, accountId))
        );
    }
}
