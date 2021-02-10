package ru.itterminal.botdesk.tickets.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import ru.itterminal.botdesk.commons.repository.EntityRepositoryWithAccount;
import ru.itterminal.botdesk.tickets.model.TicketTemplate;

@Repository
public interface TicketTemplateRepository extends EntityRepositoryWithAccount<TicketTemplate> {
    @Override
    Page<TicketTemplate> findAll(Specification<TicketTemplate> spec, Pageable pageable);
}
