package ru.itterminal.botdesk.tickets.model.spec;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.commons.model.spec.BaseSpec;
import ru.itterminal.botdesk.tickets.model.TicketStatus;

@Component
public class TicketStatusSpec  implements BaseSpec<TicketStatus, Account> {
    private static final String NAME = "name";

    public Specification<TicketStatus> getTicketStatusByNameSpec(String name) {
        return (root, query, criteriaBuilder) -> criteriaBuilder
                .like(criteriaBuilder.lower(root.get(NAME)), "%" + name.toLowerCase() + "%");
    }
}
