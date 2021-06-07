package ru.itterminal.yanmas.tickets.service.business_handler.ticket_event;

import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.tickets.model.TicketEvent;

@Component
public class SettingCurrentUserForRecipientsBeforeCreateTicketEventBusinessHandler implements EntityBusinessHandler<TicketEvent> {

    @Override
    public TicketEvent beforeCreate(TicketEvent entity, User currentUser) {
        if(entity.getRecipients() !=null
        && !entity.getRecipients().isEmpty()
        && !entity.getRecipients().contains(currentUser)) {
            entity.getRecipients().add(currentUser);
        }
        return entity;
    }
}
