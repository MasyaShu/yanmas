package ru.itterminal.yanmas.tickets.service.impl;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.CrudServiceWithBusinessHandlerImpl;
import ru.itterminal.yanmas.tickets.model.TicketEvent;
import ru.itterminal.yanmas.tickets.repository.TicketEventRepository;

import java.util.UUID;

@Service
@RequiredArgsConstructor
public class TicketEventServiceImpl extends CrudServiceWithBusinessHandlerImpl<TicketEvent, TicketEventRepository> {

    private final TicketServiceImpl ticketService;

    @Transactional(readOnly = true)
    public Page<TicketEvent> findAllByFilter(Specification<TicketEvent> specification, Pageable pageable,
                                             User currentUser, UUID ticketId) {
        ticketService.findByIdAndAccountId(ticketId, currentUser);
        return findAllByFilter(specification, pageable, currentUser);
    }
}
