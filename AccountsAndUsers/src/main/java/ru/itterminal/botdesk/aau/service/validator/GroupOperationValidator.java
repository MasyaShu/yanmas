package ru.itterminal.botdesk.aau.service.validator;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.projection.GroupUniqueFields;
import ru.itterminal.botdesk.aau.service.impl.GroupServiceImpl;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.security.jwt.JwtUser;

import java.util.List;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.checkStringForEquals;

@Slf4j
@Component
@RequiredArgsConstructor
public class GroupOperationValidator extends BasicOperationValidatorImpl<Group> {

    public static final String ACCESS_IS_DENIED_FOR_SEARCHING_BY_PASSED_GROUP_ID = "Access is denied for searching by passed groupId";
    private final GroupServiceImpl service;
    private static final String USER_FROM_AN_INNER_GROUP_CANNOT_CREATE_UPDATE_GROUPS =
            "A user from not inner group cannot create or update groups";

    @Override
    public boolean beforeCreate(Group entity) {
        checkIsInnerGroupForCreateUpdate();
        return super.beforeCreate(entity);
    }

    @Override
    public boolean beforeUpdate(Group entity) {
        checkIsInnerGroupForCreateUpdate();
        return super.beforeUpdate(entity);
    }

    @Override
    public boolean checkUniqueness(Group entity) {
        log.trace(CHECK_UNIQUENESS, entity);
        List<GroupUniqueFields> foundGroup = service.findByUniqueFields(entity);
        if (!foundGroup.isEmpty()) {
            String validatedField = "name";
            checkStringForEquals(entity.getName(), foundGroup.get(0).getName(),
                    "name", format(NOT_UNIQUE_MESSAGE, validatedField));
        }
        log.trace(FIELDS_UNIQUE, entity);
        return true;
    }

    private void checkIsInnerGroupForCreateUpdate() {
        if (!SecurityContextHolder.getContext().getAuthentication().getName().contains("anonymous")) {
            JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
            if (!jwtUser.isInnerGroup()) {
                throw new AccessDeniedException(USER_FROM_AN_INNER_GROUP_CANNOT_CREATE_UPDATE_GROUPS);
            }
        }
    }

    @Override
    public boolean checkAccessForRead(Group entity) {
        JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (jwtUser.isInnerGroup() || jwtUser.getGroupId().equals(entity.getId())) {
            return true;
        } else {
            throw new AccessDeniedException(ACCESS_IS_DENIED_FOR_SEARCHING_BY_PASSED_GROUP_ID);
        }
    }
}
