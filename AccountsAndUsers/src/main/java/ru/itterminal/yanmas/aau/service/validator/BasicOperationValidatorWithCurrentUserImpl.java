package ru.itterminal.yanmas.aau.service.validator;

import org.springframework.stereotype.Component;

import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl;

@SuppressWarnings("unused")
@Component
// TODO Delete
public class BasicOperationValidatorWithCurrentUserImpl<E extends BaseEntity> extends BasicOperationValidatorImpl<E>
        implements OperationValidatorWithCurrentUser<E> {
}
