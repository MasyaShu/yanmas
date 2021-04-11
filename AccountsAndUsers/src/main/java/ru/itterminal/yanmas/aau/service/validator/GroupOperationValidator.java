package ru.itterminal.yanmas.aau.service.validator;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.repository.GroupRepository;
import ru.itterminal.yanmas.aau.service.impl.GroupServiceImpl;
import ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.yanmas.security.jwt.JwtUser;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.checkStringForEquals;

@Slf4j
@Component
@RequiredArgsConstructor
public class GroupOperationValidator extends BasicOperationValidatorImpl<Group> {

    private final GroupServiceImpl service;
    private final GroupRepository repository;

    public static final String ACCESS_IS_DENIED_FOR_SEARCHING_BY_PASSED_GROUP_ID =
            "Access is denied for searching by passed groupId";
        private static final String USER_FROM_AN_INNER_GROUP_CANNOT_CREATE_UPDATE_GROUPS =
            "A user from not inner group cannot create or update groups";
    private static final String START_FIND_GROUP_BY_UNIQUE_FIELDS =
            "Start find user by unique fields, name: {} and not id: {} and not account: {} and not isInner: {}";

    @Override
    public boolean logicalValidationBeforeUpdate(Group entity) {
        var groupFromDataBase = service.findByIdAndAccountId(entity.getId());
        entity.setIsInner(groupFromDataBase.getIsInner());
        return super.logicalValidationBeforeUpdate(entity);
    }

    @Override
    public boolean checkUniqueness(Group entity) {
        log.trace(CHECK_UNIQUENESS, entity);
        log.trace(
                START_FIND_GROUP_BY_UNIQUE_FIELDS, entity.getName(), entity.getId(), entity.getAccount(),
                entity.getIsInner()
        );
        var foundGroup = repository.getByNameAndIsInnerAndAccount_IdAndIdNot(entity.getName(), entity.getIsInner(),
                entity.getAccount().getId(), entity.getId()
        );
        if (!foundGroup.isEmpty()) {
            String validatedField = "name";
            checkStringForEquals(entity.getName(), foundGroup.get(0).getName(),
                    "name", format(NOT_UNIQUE_MESSAGE, validatedField)
            );
        }
        log.trace(FIELDS_UNIQUE, entity);
        return true;
    }

    private void checkIsInnerGroupForCreateUpdate() {
        if (!SecurityContextHolder.getContext().getAuthentication().getName().contains(ANONYMOUS)) {
            JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
            if (!jwtUser.isInnerGroup()) {
                throw new AccessDeniedException(USER_FROM_AN_INNER_GROUP_CANNOT_CREATE_UPDATE_GROUPS);
            }
        }
    }

    @Override
    public void checkAccessBeforeCreate(Group entity) {
        checkIsInnerGroupForCreateUpdate();
    }

    @Override
    public void checkAccessBeforeUpdate(Group entity) {
        checkIsInnerGroupForCreateUpdate();
    }

    @Override
    public void checkAccessBeforeRead(Group entity) {
        if (!SecurityContextHolder.getContext().getAuthentication().getName().contains(ANONYMOUS)) {
            JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
            if (!((jwtUser.isInnerGroup() && jwtUser.getWeightRole() > Roles.AUTHOR.getWeight())
                    || jwtUser.getGroupId().equals(entity.getId()))) {
                throw new AccessDeniedException(ACCESS_IS_DENIED_FOR_SEARCHING_BY_PASSED_GROUP_ID);
            }
        }
    }
}
