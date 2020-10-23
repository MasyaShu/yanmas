package ru.itterminal.botdesk.aau.service.impl;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.projection.GroupUniqueFields;
import ru.itterminal.botdesk.aau.model.projection.UserUniqueFields;
import ru.itterminal.botdesk.aau.repository.GroupRepository;
import ru.itterminal.botdesk.aau.repository.UserRepository;
import ru.itterminal.botdesk.aau.service.validator.GroupOperationValidator;
import ru.itterminal.botdesk.aau.service.validator.UserOperationValidator;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceImpl;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static java.lang.String.format;

@Slf4j
@Service
@Transactional
public class GroupServiceImpl extends CrudServiceImpl<Group, GroupOperationValidator, GroupRepository> {

    public static final String NOT_FOUND_USERS_BY_UNIQUE_FIELDS_USER_IS_NULL =
            "Not found groups by unique fields, group is null";
    public static final String NOT_FOUND_USERS_BY_UNIQUE_FIELDS_ACCOUNT_IS_NULL =
            "Not found groups by unique fields, account is null";
    public static final String NOT_FOUND_USERS_BY_UNIQUE_FIELDS_NAME_IS_NULL =
            "Not found groups by unique fields, name is null";
    public static final String NOT_FOUND_USERS_BY_UNIQUE_FIELDS_ID_IS_NULL =
            "Not found groups by unique fields, id is null";
    public static final String START_FIND_USER_BY_UNIQUE_FIELDS =
            "Start find user by unique fields, name: {} and not id: {} and not account: {}";

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
        catch (ObjectOptimisticLockingFailureException ex) {
            throw new OptimisticLockingFailureException(format(VERSION_INVALID_MESSAGE, entity.getId()));
        }
    }

    public List<GroupUniqueFields> findByUniqueFields(Group group) {
        if (group == null) {
            log.error(NOT_FOUND_USERS_BY_UNIQUE_FIELDS_USER_IS_NULL);
            throw new EntityNotExistException(NOT_FOUND_USERS_BY_UNIQUE_FIELDS_USER_IS_NULL);
        }
        if (group.getName() == null) {
            log.error(NOT_FOUND_USERS_BY_UNIQUE_FIELDS_NAME_IS_NULL);
            throw new EntityNotExistException(NOT_FOUND_USERS_BY_UNIQUE_FIELDS_NAME_IS_NULL);
        }
        if (group.getId() == null) {
            log.error(NOT_FOUND_USERS_BY_UNIQUE_FIELDS_ID_IS_NULL);
            throw new EntityNotExistException(NOT_FOUND_USERS_BY_UNIQUE_FIELDS_ID_IS_NULL);
        }
        if (group.getAccount() == null) {
            log.error(NOT_FOUND_USERS_BY_UNIQUE_FIELDS_ACCOUNT_IS_NULL);
            throw new EntityNotExistException(NOT_FOUND_USERS_BY_UNIQUE_FIELDS_ACCOUNT_IS_NULL);
        }
        log.trace(START_FIND_USER_BY_UNIQUE_FIELDS, group.getName(), group.getId(), group.getAccount());
        return repository.getByNameAndAccount_IdAndIdNot(group.getName(), group.getAccount().getId(),  group.getId());
    }
}
