package ru.itterminal.botdesk.aau.service.impl;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.chekObjectForNull;

import java.util.List;

import javax.persistence.OptimisticLockException;

import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.projection.GroupUniqueFields;
import ru.itterminal.botdesk.aau.repository.GroupRepository;
import ru.itterminal.botdesk.aau.service.validator.GroupOperationValidator;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceWithAccountImpl;

@Slf4j
@Service
@Transactional
public class GroupServiceImpl extends CrudServiceWithAccountImpl<Group, GroupOperationValidator, GroupRepository> {

    private static final String NOT_FOUND_GROUPS_BY_UNIQUE_FIELDS_GROUP_IS_NULL =
            "Not found groups by unique fields, group is null";
    private static final String NOT_FOUND_GROUPS_BY_UNIQUE_FIELDS_ACCOUNT_IS_NULL =
            "Not found groups by unique fields, account is null";
    private static final String NOT_FOUND_GROUPS_BY_UNIQUE_FIELDS_NAME_IS_NULL =
            "Not found groups by unique fields, name is null";
    private static final String NOT_FOUND_GROUPS_BY_UNIQUE_FIELDS_ID_IS_NULL =
            "Not found groups by unique fields, id is null";
    private static final String START_FIND_GROUP_BY_UNIQUE_FIELDS =
            "Start find user by unique fields, name: {} and not id: {} and not account: {}";

    public static final String ENTITY_GROUP_NAME = Group.class.getSimpleName();

    @Override
    public Group update(Group entity) {
        validator.beforeUpdate(entity);
        log.trace(format(UPDATE_INIT_MESSAGE, entity.getClass().getSimpleName(), entity.getId(), entity));
        Group entityFromDatabase = repository.findById(entity.getId()).orElseThrow(() -> {
            String message = format(ENTITY_NOT_EXIST_MESSAGE, entity.getClass().getSimpleName(), entity.getId());
            log.error(message);
            return new EntityNotExistException(message);
        });
        if (entity.getComment() == null) {
            entity.setComment(entityFromDatabase.getComment());
        }
        try {
            entity.setIsInner(entityFromDatabase.getIsInner());
            Group updatedEntity = repository.update(entity);
            log.trace(format(UPDATE_FINISH_MESSAGE, entity.getClass().getSimpleName(), entity.getId(), updatedEntity));
            return updatedEntity;
        }
        catch (OptimisticLockException | ObjectOptimisticLockingFailureException ex) {
            throw new OptimisticLockingFailureException(format(VERSION_INVALID_MESSAGE, entity.getId()));
        }
    }

    public List<GroupUniqueFields> findByUniqueFields(Group group) {
        chekObjectForNull(group, NOT_FOUND_GROUPS_BY_UNIQUE_FIELDS_GROUP_IS_NULL, EntityNotExistException.class);
        chekObjectForNull(group.getName(), NOT_FOUND_GROUPS_BY_UNIQUE_FIELDS_NAME_IS_NULL,
                          EntityNotExistException.class
        );
        chekObjectForNull(group.getId(), NOT_FOUND_GROUPS_BY_UNIQUE_FIELDS_ID_IS_NULL, EntityNotExistException.class);
        chekObjectForNull(group.getAccount(), NOT_FOUND_GROUPS_BY_UNIQUE_FIELDS_ACCOUNT_IS_NULL,
                          EntityNotExistException.class
        );
        log.trace(START_FIND_GROUP_BY_UNIQUE_FIELDS, group.getName(), group.getId(), group.getAccount());
        return repository.getByNameAndAccount_IdAndIdNot(group.getName(), group.getAccount().getId(), group.getId());
    }
}
