package ru.itterminal.yanmas.tickets.repository;

import org.springframework.stereotype.Repository;
import ru.itterminal.yanmas.commons.repository.EntityRepositoryWithAccount;
import ru.itterminal.yanmas.tickets.model.TicketEvent;

@Repository
public interface TicketEventRepository extends EntityRepositoryWithAccount<TicketEvent> {
}
