package ru.itterminal.yanmas.aau.service.validator;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.checkStringForEquals;

import java.util.List;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.projection.GroupUniqueFields;
import ru.itterminal.yanmas.aau.service.impl.GroupServiceImpl;
import ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.yanmas.security.jwt.JwtUser;

@Slf4j
@Component
@RequiredArgsConstructor
public class GroupOperationValidator extends BasicOperationValidatorImpl<Group> {

    public static final String ACCESS_IS_DENIED_FOR_SEARCHING_BY_PASSED_GROUP_ID =
            "Access is denied for searching by passed groupId";
    private final GroupServiceImpl service;
    private static final String USER_FROM_AN_INNER_GROUP_CANNOT_CREATE_UPDATE_GROUPS =
            "A user from not inner group cannot create or update groups";

    @Override
    public boolean logicalValidationBeforeUpdate(Group entity) {
        var groupFromDataBase = service.findByIdAndAccountId(entity.getId());
        entity.setIsInner(groupFromDataBase.getIsInner());
        return super.logicalValidationBeforeUpdate(entity);
    }

    @Override
    public boolean checkUniqueness(Group entity) {
        log.trace(CHECK_UNIQUENESS, entity);
        List<GroupUniqueFields> foundGroup = service.findByUniqueFields(entity);
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
