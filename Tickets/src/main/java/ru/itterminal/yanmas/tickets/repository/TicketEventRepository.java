package ru.itterminal.yanmas.tickets.repository;

import org.springframework.stereotype.Repository;
import ru.itterminal.yanmas.commons.repository.EntityRepositoryWithAccount;
import ru.itterminal.yanmas.tickets.model.TicketEvent;

import java.util.List;
import java.util.UUID;

@Repository
public interface TicketEventRepository extends EntityRepositoryWithAccount<TicketEvent> {
    List<TicketEvent> getByAccount_IdAndTicketId(UUID accountId, UUID ticketId);
}
