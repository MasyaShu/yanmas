package ru.itterminal.yanmas.aau.service.validator;

import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.commons.service.validator.OperationValidator;

@SuppressWarnings("unused")
public interface OperationValidatorWithCurrentUser<E extends BaseEntity> extends OperationValidator<E> {

    default void checkAccessBeforeRead(E entity, User currentUser) {}

    default void checkAccessBeforeCreate(E entity, User currentUser) {}

    default void checkAccessBeforeUpdate(E entity, User currentUser) {}

}
