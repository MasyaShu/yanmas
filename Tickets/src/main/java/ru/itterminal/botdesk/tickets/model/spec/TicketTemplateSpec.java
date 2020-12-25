package ru.itterminal.botdesk.tickets.model.spec;

import lombok.val;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.commons.model.spec.BaseSpec;
import ru.itterminal.botdesk.tickets.model.TicketTemplate;
import ru.itterminal.botdesk.tickets.model.TicketType;

import javax.persistence.criteria.Join;
import javax.persistence.criteria.Predicate;
import java.util.List;
import java.util.UUID;

@Component
public class TicketTemplateSpec implements BaseSpec<TicketTemplate, Account> {

    public static final String SUBJECT = "subject";
    public static final String DESCRIPTION = "description";
    public static final String DATE_START = "dateStart";
    public static final String DATE_END = "dateEnd";
    public static final String TICKET_IN_WORK = "isOnlyOneTicketInWork";
    public static final String IS_ACTIVE = "isActive";
    public static final String AUTHOR = "author";
    public static final String TICKET_TYPE = "ticketType";

    @SuppressWarnings("DuplicatedCode")
    public Specification<TicketTemplate> getTicketTemplateBySubjectSpec(String subject) {
        return (root, query, criteriaBuilder) -> criteriaBuilder
                .like(criteriaBuilder.lower(root.get(SUBJECT)), "%" + subject.toLowerCase() + "%");
    }

    @SuppressWarnings("DuplicatedCode")
    public Specification<TicketTemplate> getTicketTemplateByDescriptionSpec(String description) {
        return (root, query, criteriaBuilder) -> {
            val objectPathComment = root.get(DESCRIPTION);
            if (description.isEmpty()) {
                Predicate predicateForNull = criteriaBuilder.isNull(objectPathComment);
                Predicate predicateForEmpty = criteriaBuilder.equal(objectPathComment, EMPTY_STRING);
                return criteriaBuilder.or(predicateForEmpty, predicateForNull);
            }
            return criteriaBuilder
                    .like(criteriaBuilder.lower(root.get(DESCRIPTION)), "%" + description.toLowerCase() + "%");
        };
    }

    public Specification<TicketTemplate> getTicketTemplateByDateEndSpec(long date, String comparison) {
        return getTicketTemplateByDateSpec(date, comparison, DATE_END);
    }

    public Specification<TicketTemplate> getTicketTemplateByDateStartSpec(long date, String comparison) {
        return getTicketTemplateByDateSpec(date, comparison, DATE_START);
    }

    private Specification<TicketTemplate> getTicketTemplateByDateSpec(long date, String comparison, String field) {
        switch (comparison) {
            case ">=" -> {
                return (root, query, criteriaBuilder) -> criteriaBuilder
                        .ge(root.get(field), date);
            }
            case ">" -> {
                return (root, query, criteriaBuilder) -> criteriaBuilder
                        .gt(root.get(field), date);
            }
            case "<=" -> {
                return (root, query, criteriaBuilder) -> criteriaBuilder
                        .le(root.get(field), date);
            }
            case "<" -> {
                return (root, query, criteriaBuilder) -> criteriaBuilder
                        .lt(root.get(field), date);
            }
            default -> {
                return (root, query, criteriaBuilder) -> criteriaBuilder
                        .equal(root.get(field), date);
            }
        }
    }

    public Specification<TicketTemplate> getTicketTemplatesByIsOnlyOneTicketInWorkSpec(Boolean isOnlyOneTicketInWork) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get(TICKET_IN_WORK), isOnlyOneTicketInWork);
    }

    public Specification<TicketTemplate> getTicketTemplatesByIsActiveSpec(Boolean isActive) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get(IS_ACTIVE), isActive);
    }

    @SuppressWarnings("DuplicatedCode")
    public Specification<TicketTemplate> getTicketTemplateByListOfAuthorsSpec(List<UUID> listAuthorId) {
        return (root, query, criteriaBuilder) -> {
            Join<TicketTemplate, User> ticketTemplateJoin = root.join(AUTHOR);
            Predicate returnedPredicate = criteriaBuilder.equal(ticketTemplateJoin.<UUID> get("id"), listAuthorId.get(0));
            for (int i = 1; i < listAuthorId.size(); i++) {
                Predicate predicate = criteriaBuilder.equal(ticketTemplateJoin.<UUID> get("id"), listAuthorId.get(i));
                returnedPredicate = criteriaBuilder.or(returnedPredicate, predicate);
            }
            return returnedPredicate;
        };
    }

    @SuppressWarnings("DuplicatedCode")
    public Specification<TicketTemplate> getTicketTemplateByListOfTicketTemplatesSpec(List<UUID> listTicketTypeId) {
        return (root, query, criteriaBuilder) -> {
            Join<TicketTemplate, TicketType> ticketTemplateJoin = root.join(TICKET_TYPE);
            Predicate returnedPredicate = criteriaBuilder.equal(ticketTemplateJoin.<UUID> get("id"), listTicketTypeId.get(0));
            for (int i = 1; i < listTicketTypeId.size(); i++) {
                Predicate predicate = criteriaBuilder.equal(ticketTemplateJoin.<UUID> get("id"), listTicketTypeId.get(i));
                returnedPredicate = criteriaBuilder.or(returnedPredicate, predicate);
            }
            return returnedPredicate;
        };
    }

}
