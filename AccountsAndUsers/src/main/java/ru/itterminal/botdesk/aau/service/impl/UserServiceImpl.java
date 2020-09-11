package ru.itterminal.botdesk.aau.service.impl;

import static java.lang.String.format;

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

    public UserUniqueFields findByUniqueFields(User user) {
        return repository.getByEmailAndIdNot(user.getEmail(), user.getId());
    }
}
