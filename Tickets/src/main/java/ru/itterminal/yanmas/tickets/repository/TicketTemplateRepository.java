package ru.itterminal.yanmas.tickets.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Repository;
import ru.itterminal.yanmas.commons.repository.EntityRepositoryWithAccount;
import ru.itterminal.yanmas.tickets.model.TicketTemplate;

@Repository
public interface TicketTemplateRepository extends EntityRepositoryWithAccount<TicketTemplate> {
    @SuppressWarnings("NullableProblems")
    @Override
    Page<TicketTemplate> findAll(Specification<TicketTemplate> spec, Pageable pageable);
}
