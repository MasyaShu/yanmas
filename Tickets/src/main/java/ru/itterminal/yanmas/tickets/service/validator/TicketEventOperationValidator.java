package ru.itterminal.yanmas.tickets.service.validator;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.validator.BasicOperationValidatorWithCurrentUserImpl;
import ru.itterminal.yanmas.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.yanmas.tickets.model.TicketEvent;

@Slf4j
@Component
@RequiredArgsConstructor
public class TicketEventOperationValidator extends BasicOperationValidatorWithCurrentUserImpl<TicketEvent> {

    @Override
    public void checkAccessBeforeRead(TicketEvent entity, User currentUser) {
        super.checkAccessBeforeRead(entity, currentUser);
    }

    @Override
    public boolean logicalValidationBeforeCreate(TicketEvent entity) {
        // TODO ошибка если все пустое (нет обновления тикета, нет файлов, нет комментария)
        return super.logicalValidationBeforeCreate(entity);
    }
}
