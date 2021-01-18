package ru.itterminal.botdesk.tickets.repository;

import org.springframework.stereotype.Repository;

import ru.itterminal.botdesk.commons.repository.EntityRepositoryWithAccount;
import ru.itterminal.botdesk.tickets.model.Ticket;

@Repository
public interface TicketRepository extends EntityRepositoryWithAccount<Ticket> {

}