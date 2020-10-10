package ru.itterminal.botdesk.aau.service.impl;

import static java.lang.String.format;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.projection.UserUniqueFields;
import ru.itterminal.botdesk.aau.repository.UserRepository;
import ru.itterminal.botdesk.aau.service.validator.UserOperationValidator;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceImpl;

@Slf4j
@Service
@Transactional
public class UserServiceImpl extends CrudServiceImpl<User, UserOperationValidator, UserRepository> {

    BCryptPasswordEncoder encoder;

    @Autowired
    public UserServiceImpl(BCryptPasswordEncoder encoder) {
        this.encoder = encoder;
    }

    public List<UserUniqueFields> findByUniqueFields(User user) {
        return repository.getByEmailAndIdNot(user.getEmail(), user.getId());
    }

    @Override
    public User create(User entity) {
        entity.setPassword(encoder.encode(entity.getPassword()));
        if (entity.getLanguage() == null) {
            String ln = entity.getAccount().getLanguage();
            entity.setLanguage(ln);
        }
        return super.create(entity);
    }

    @Override
    public User update(User entity) {
        validator.beforeUpdate(entity);
        log.trace(format(UPDATE_INIT_MESSAGE, entity.getClass().getSimpleName(), entity.getId(), entity));
        User entityFromDatabase = super.findById(entity.getId());
        if (entityFromDatabase == null) {
            String message = format(ENTITY_NOT_EXIST_MESSAGE, entity.getClass().getSimpleName(), entity.getId());
            log.error(message);
            throw new EntityNotExistException(message);
        }
        if (!entity.getPassword().isEmpty()) {
            entity.setPassword(encoder.encode(entity.getPassword()));
        } else {
            entity.setPassword(entityFromDatabase.getPassword());
        }
        try {
            User updatedEntity = repository.update(entity);
            log.trace(format(UPDATE_FINISH_MESSAGE, entity.getClass().getSimpleName(), entity.getId(), updatedEntity));
            return updatedEntity;
        }
        catch (ObjectOptimisticLockingFailureException ex) {
            throw new OptimisticLockingFailureException(format(VERSION_INVALID_MESSAGE, entity.getId()));
        }
    }

    public Optional<User> findByEmail (String email) {
        return  repository.getByEmail(email);
    }
}
