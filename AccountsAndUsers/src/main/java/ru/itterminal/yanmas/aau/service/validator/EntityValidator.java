package ru.itterminal.yanmas.aau.service.validator;

import java.util.List;
import java.util.Map;

import org.springframework.security.access.AccessDeniedException;

import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.commons.model.BaseEntity;

@SuppressWarnings("unused")
public interface EntityValidator<E extends BaseEntity> {

    String NOT_UNIQUE_MESSAGE = "%s is occupied";
    String NOT_UNIQUE_CODE = "not unique";
    String THIS_KEY_OF_SETTINGS_ACCOUNT_ID_GROUP_ID_USER_ID =
            "This key of settings (accountId, groupId, userId)";
    String USER_FROM_OUTER_GROUP_CANNOT_CREATE_OR_UPDATE_OR_READ_THIS_ENTITY =
            "A user from outer group cannot access to this entity";
    String ACCESS_IS_DENIED_FOR_SEARCHING_BY_PASSED_GROUP_ID =
            "Access is denied for searching by passed groupId";
    String THIS_NAME =
            "This name";

    default void logicalValidationBeforeCreate(E entity, Map<String, List<ValidationError>> errors) {}

    default  void logicalValidationBeforeUpdate(E entity, Map<String, List<ValidationError>> errors) {}

    default void checkAccessBeforeRead(User currentUser) {}

    default void checkAccessBeforeRead(E entity, User currentUser) {}

    default void checkAccessBeforeCreate(User currentUser) {}

    default void checkAccessBeforeCreate(E entity, User currentUser) {}

    default void checkAccessBeforeUpdate(User currentUser) {}

    default void checkAccessBeforeUpdate(E entity, User currentUser) {}

    default void throwAccessDeniedExceptionIfCurrentUserFromOuterGroup (User currentUser) {
        if (Boolean.FALSE.equals(currentUser.getGroup().getIsInner())) {
            throw new AccessDeniedException(USER_FROM_OUTER_GROUP_CANNOT_CREATE_OR_UPDATE_OR_READ_THIS_ENTITY);
        }
    }
}
