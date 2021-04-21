package ru.itterminal.yanmas.tickets.service.validator;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;
import ru.itterminal.yanmas.tickets.model.GroupTicketTypes;
import ru.itterminal.yanmas.tickets.repository.GroupTicketTypesRepository;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.createLogicalValidationException;

@Slf4j
@Component
@RequiredArgsConstructor
public class GroupTicketTypesOperationValidator extends BasicOperationValidatorImpl<GroupTicketTypes> {

    private final JwtUserBuilder jwtUserBuilder;
    private final GroupTicketTypesRepository repository;

    private static final String NAME = "name";
    public static final String START_FIND_GROUP_OF_TICKET_TYPES_BY_UNIQUE_FIELDS_NAME_ACCOUNT =
            "Start find group of ticket types by unique fields, name: {} account: {}";

    @Override
    public boolean checkUniqueness(GroupTicketTypes entity) {
        log.trace(CHECK_UNIQUENESS, entity);
        log.trace(START_FIND_GROUP_OF_TICKET_TYPES_BY_UNIQUE_FIELDS_NAME_ACCOUNT,
                entity.getName(), entity.getAccount()
        );
        var groupTicketTypesList = repository
                .getByNameAndAccount_IdAndIdNot(
                        entity.getName(),
                        entity.getAccount().getId(),
                        entity.getId()
                );
        if (!groupTicketTypesList.isEmpty()) {
            log.error(format(NOT_UNIQUE_MESSAGE, NAME));
            throw createLogicalValidationException(NOT_UNIQUE_CODE, format(NOT_UNIQUE_MESSAGE, NAME));
        }
        log.trace(FIELDS_UNIQUE, entity);
        return true;
    }

    @Override
    public void checkAccessBeforeCreate(GroupTicketTypes entity) {
        jwtUserBuilder.throwAccessDeniedExceptionIfCurrentUserFromOuterGroup();
    }

    @Override
    public void checkAccessBeforeUpdate(GroupTicketTypes entity) {
        jwtUserBuilder.throwAccessDeniedExceptionIfCurrentUserFromOuterGroup();
    }
}
