package ru.itterminal.yanmas.tickets.service.business_handler;

import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.tickets.model.TicketStatus;

@Component
public class TicketStatusBusinessHandler implements EntityBusinessHandler<TicketStatus> {

    @Override
    public void beforeCreate(TicketStatus entity, User currentUser) {
        entity.setDeleted(false);
        if(entity.getIsCanceledPredefined() == null) {
            entity.setIsCanceledPredefined(false);
        }
        if(entity.getIsFinishedPredefined() == null) {
            entity.setIsFinishedPredefined(false);
        }
        if(entity.getIsReopenedPredefined() == null) {
            entity.setIsReopenedPredefined(false);
        }
        if(entity.getIsStartedPredefined() == null) {
            entity.setIsStartedPredefined(false);
        }
    }
}
