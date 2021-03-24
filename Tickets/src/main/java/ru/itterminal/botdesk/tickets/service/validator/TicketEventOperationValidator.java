package ru.itterminal.botdesk.tickets.service.validator;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;
import ru.itterminal.botdesk.tickets.model.TicketEvent;

@Slf4j
@Component
@RequiredArgsConstructor
public class TicketEventOperationValidator extends BasicOperationValidatorImpl<TicketEvent> {

    @Override
    public boolean beforeCreate(TicketEvent entity) {
        // TODO ошибка если все пустое (нет обновления тикета, нет файлов, нет комментария)
        return super.beforeCreate(entity);
    }
}