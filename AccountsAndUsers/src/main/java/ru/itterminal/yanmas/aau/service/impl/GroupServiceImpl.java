package ru.itterminal.yanmas.aau.service.impl;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.projection.GroupUniqueFields;
import ru.itterminal.yanmas.aau.repository.GroupRepository;
import ru.itterminal.yanmas.aau.service.validator.GroupOperationValidator;

@Slf4j
@Service
@Transactional
@RequiredArgsConstructor
public class GroupServiceImpl extends CrudServiceWithAccountImpl<Group, GroupOperationValidator, GroupRepository> {

    private static final String START_FIND_GROUP_BY_UNIQUE_FIELDS =
            "Start find user by unique fields, name: {} and not id: {} and not account: {} and not isInner: {}";

    @Transactional(readOnly = true)
    public List<GroupUniqueFields> findByUniqueFields(Group group) {
        log.trace(
                START_FIND_GROUP_BY_UNIQUE_FIELDS, group.getName(), group.getId(), group.getAccount(),
                group.getIsInner()
        );
        return repository.getByNameAndIsInnerAndAccount_IdAndIdNot(group.getName(), group.getIsInner(),
                                                                   group.getAccount().getId(), group.getId()
        );
    }
}
