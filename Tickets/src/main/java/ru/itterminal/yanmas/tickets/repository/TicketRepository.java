package ru.itterminal.yanmas.tickets.repository;

import org.springframework.stereotype.Repository;

import ru.itterminal.yanmas.commons.repository.EntityRepositoryWithAccount;
import ru.itterminal.yanmas.tickets.model.Ticket;

@Repository
public interface TicketRepository extends EntityRepositoryWithAccount<Ticket> {

}