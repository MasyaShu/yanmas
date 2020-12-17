package ru.itterminal.botdesk.tickets.model.spec;

import lombok.val;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.commons.model.spec.BaseSpec;
import ru.itterminal.botdesk.tickets.model.TicketType;

import javax.persistence.criteria.Predicate;

@Component
public class TicketTypeSpec implements BaseSpec<TicketType, Account> {
    private static final String COMMENT = "comment";
    private static final String NAME = "name";
    private static final String IS_PREDEFINED = "isPredefined";
    private static final String EMPTY_STRING = "";


    public Specification<TicketType> getTicketTypesByNameSpec(String name) {
        return (root, query, criteriaBuilder) -> criteriaBuilder
                .like(criteriaBuilder.lower(root.get(NAME)), "%" + name.toLowerCase() + "%");
    }

    @SuppressWarnings("DuplicatedCode")
    public Specification<TicketType> getTicketTypesByCommentSpec(String comment) {
        return (root, query, criteriaBuilder) -> {
            val objectPathComment = root.get(COMMENT);
            if (comment.isEmpty()) {
                Predicate predicateForNull =  criteriaBuilder.isNull(objectPathComment);
                Predicate predicateForEmpty =  criteriaBuilder.equal(objectPathComment, EMPTY_STRING);
                return criteriaBuilder.or(predicateForEmpty, predicateForNull);
            }
            return  criteriaBuilder
                    .like(criteriaBuilder.lower(root.get(COMMENT)), "%" + comment.toLowerCase() + "%");
        };
    }

    public Specification<TicketType> getTicketTypesByIsPredefinedSpec(Boolean isDeprecated) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get(IS_PREDEFINED), isDeprecated);
    }
}
