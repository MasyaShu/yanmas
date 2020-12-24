package ru.itterminal.botdesk.aau.service.impl;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.projection.GroupUniqueFields;
import ru.itterminal.botdesk.aau.repository.GroupRepository;
import ru.itterminal.botdesk.aau.service.validator.GroupOperationValidator;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceWithAccountImpl;

import java.util.List;

@Slf4j
@Service
@Transactional
public class GroupServiceImpl extends CrudServiceWithAccountImpl<Group, GroupOperationValidator, GroupRepository> {

    private static final String START_FIND_GROUP_BY_UNIQUE_FIELDS =
            "Start find user by unique fields, name: {} and not id: {} and not account: {} and not isInner: {}";

    @Transactional(readOnly = true)
    public List<GroupUniqueFields> findByUniqueFields(Group group) {
        log.trace(START_FIND_GROUP_BY_UNIQUE_FIELDS, group.getName(), group.getId(), group.getAccount(), group.getIsInner());
        return repository.getByNameAndIsInnerAndAccount_IdAndIdNot(group.getName(), group.getIsInner(), group.getAccount().getId(), group.getId());
    }
}
