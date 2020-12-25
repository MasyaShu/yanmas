package ru.itterminal.botdesk.tickets.repository;

import org.springframework.stereotype.Repository;

import ru.itterminal.botdesk.commons.repository.CustomizedParentEntityRepository;
import ru.itterminal.botdesk.tickets.model.TicketCounter;

@Repository
public interface TicketCounterRepository extends CustomizedParentEntityRepository<TicketCounter> {

}
