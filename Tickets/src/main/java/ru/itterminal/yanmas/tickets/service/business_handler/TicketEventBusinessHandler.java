package ru.itterminal.yanmas.tickets.service.business_handler;

import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.tickets.model.TicketEvent;

@Component
public class TicketEventBusinessHandler implements EntityBusinessHandler<TicketEvent> {
    @Override
    public void beforeCreate(TicketEvent entity, User currentUser) {
        EntityBusinessHandler.super.beforeCreate(entity, currentUser);
    }
}
