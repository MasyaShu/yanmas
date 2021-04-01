package ru.itterminal.yanmas.tickets.repository;

import org.springframework.stereotype.Repository;

import ru.itterminal.yanmas.commons.repository.CustomizedParentEntityRepository;
import ru.itterminal.yanmas.tickets.model.TicketCounter;

@Repository
public interface TicketCounterRepository extends CustomizedParentEntityRepository<TicketCounter> {

}
