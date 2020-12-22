package ru.itterminal.botdesk.aau.service.validator;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.projection.GroupUniqueFields;
import ru.itterminal.botdesk.aau.service.impl.GroupServiceImpl;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.security.jwt.JwtUser;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static java.lang.String.format;
import static java.util.Collections.singletonList;

@Slf4j
@Component
@RequiredArgsConstructor
public class GroupOperationValidator extends BasicOperationValidatorImpl<Group> {

    private final GroupServiceImpl service;
    private static final String INNER_GROUP = "Inner group";
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

    @SuppressWarnings("DuplicatedCode")
    @Override
    public boolean checkUniqueness(Group entity) {
        log.trace(CHECK_UNIQUENESS, entity);
        Map<String, List<ValidationError>> errors = new HashMap<>();
        List<GroupUniqueFields> foundGroup = service.findByUniqueFields(entity);
        if (foundGroup.isEmpty()) {
            log.trace(FIELDS_UNIQUE, entity);
            return true;
        } else {
            String validatedField;
            if (entity.getName().equalsIgnoreCase(foundGroup.get(0).getName())) {
                validatedField = "name";
                errors.put(validatedField, singletonList(new ValidationError(NOT_UNIQUE_CODE, format(NOT_UNIQUE_MESSAGE, validatedField))));
            }
            log.error(FIELDS_NOT_UNIQUE, errors);
            throw new LogicalValidationException(VALIDATION_FAILED, errors);
        }
    }

    private void checkIsInnerGroupForCreateUpdate() {
        Map<String, List<ValidationError>> errors = new HashMap<>();
        if (!SecurityContextHolder.getContext().getAuthentication().getName().contains("anonymous")) {
            JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
            errors = new HashMap<>();
            if (!jwtUser.isInnerGroup()) {
                errors.put(INNER_GROUP, singletonList(new ValidationError(LOGIC_CONSTRAINT_CODE,
                        USER_FROM_AN_INNER_GROUP_CANNOT_CREATE_UPDATE_GROUPS)));
            }
        }
        if (!errors.isEmpty()) {
            log.error(FIELDS_ARE_NOT_VALID, errors);
            throw new LogicalValidationException(VALIDATION_FAILED, errors);
        }
    }

}
