package ru.itterminal.botdesk.tickets.service.validator;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.tickets.model.GroupTicketTypes;

@Slf4j
@Component
@RequiredArgsConstructor
public class GroupTicketTypesOperationValidator extends BasicOperationValidatorImpl<GroupTicketTypes> {

//    private static final String NAME = "name";
//    public static final String A_USER_FROM_NOT_INNER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_STATUS =
//            "A user from not inner group cannot create or update ticket status";
//    private final TicketStatusServiceImpl service;
//
//    @Override
//    public boolean checkUniqueness(TicketStatus entity) {
//        log.trace(CHECK_UNIQUENESS, entity);
//        List<TicketStatusUniqueFields> foundTicketStatus = service.findByUniqueFields(entity);
//        if (!foundTicketStatus.isEmpty()) {
//            String validatedField = NAME;
//            checkStringForEquals(entity.getName(), foundTicketStatus.get(0).getName(),
//                    validatedField, format(NOT_UNIQUE_MESSAGE, validatedField));
//        }
//        log.trace(FIELDS_UNIQUE, entity);
//        return true;
//    }
//
//    private void checkIsInnerGroupForCreateUpdate() {
//        if (!SecurityContextHolder.getContext().getAuthentication().getName().contains("anonymous")) {
//            JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
//            if (!jwtUser.isInnerGroup()) {
//                throw new AccessDeniedException(A_USER_FROM_NOT_INNER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_STATUS);
//            }
//        }
//    }
//
//    @Override
//    public void checkAccessBeforeCreate(TicketStatus entity) {
//        checkIsInnerGroupForCreateUpdate();
//    }
//
//    @Override
//    public void checkAccessBeforeUpdate(TicketStatus entity) {
//        checkIsInnerGroupForCreateUpdate();
//    }
}
