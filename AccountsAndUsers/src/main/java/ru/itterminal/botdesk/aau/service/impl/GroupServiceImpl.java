package ru.itterminal.botdesk.aau.service.impl;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
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

    public List<GroupUniqueFields> findByUniqueFields(Group group) {
        return repository.getByNameAndIdNot(group.getName(), group.getId());
    }
}
