package ru.itterminal.yanmas.tickets.service.validator;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.yanmas.security.jwt.JwtUser;
import ru.itterminal.yanmas.tickets.model.TicketStatus;
import ru.itterminal.yanmas.tickets.repository.TicketStatusRepository;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.util.CommonMethodsForValidation.checkStringForEquals;

@Slf4j
@Component
@RequiredArgsConstructor
public class TicketStatusOperationValidator extends BasicOperationValidatorImpl<TicketStatus> {

    private static final String NAME = "name";
    public static final String A_USER_FROM_NOT_INNER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_STATUS =
            "A user from not inner group cannot create or update ticket status";
    private static final String START_FIND_TICKET_TYPES_BY_UNIQUE_FIELDS =
            "Start find ticket status by unique fields, name: {} and not id: {} and not account: {}";
    private final TicketStatusRepository repository;

    @Override
    public boolean checkUniqueness(TicketStatus entity) {
        log.trace(CHECK_UNIQUENESS, entity);
        log.trace(START_FIND_TICKET_TYPES_BY_UNIQUE_FIELDS, entity.getName(), entity.getId(), entity.getAccount());
        var foundTicketStatus = repository.getByNameAndAccount_IdAndIdNot(entity.getName(), entity.getAccount().getId(), entity
                .getId());
        if (!foundTicketStatus.isEmpty()) {
            String validatedField = NAME;
            checkStringForEquals(entity.getName(), foundTicketStatus.get(0).getName(),
                    validatedField, format(NOT_UNIQUE_MESSAGE, validatedField));
        }
        log.trace(FIELDS_UNIQUE, entity);
        return true;
    }

    private void checkIsInnerGroupForCreateUpdate() {
        if (!SecurityContextHolder.getContext().getAuthentication().getName().contains("anonymous")) {
            JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
            if (!jwtUser.isInnerGroup()) {
                throw new AccessDeniedException(A_USER_FROM_NOT_INNER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_STATUS);
            }
        }
    }

    @Override
    public void checkAccessBeforeCreate(TicketStatus entity) {
        checkIsInnerGroupForCreateUpdate();
    }

    @Override
    public void checkAccessBeforeUpdate(TicketStatus entity) {
        checkIsInnerGroupForCreateUpdate();
    }
}