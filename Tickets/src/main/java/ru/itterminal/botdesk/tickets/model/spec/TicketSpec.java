package ru.itterminal.botdesk.tickets.model.spec;

import java.util.List;
import java.util.UUID;

import javax.persistence.criteria.ListJoin;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Subquery;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.commons.model.spec.BaseSpec;
import ru.itterminal.botdesk.tickets.model.Ticket;

@SuppressWarnings({"unused", "DuplicatedCode"})
@Component
public class TicketSpec implements BaseSpec<Ticket, Account> {

    public static final String AUTHOR = "author";
    public static final String NUMBER = "number";
    public static final String CREATED_AT = "createdAt";
    public static final String SUBJECT = "subject";
    public static final String DESCRIPTION = "description";
    public static final String IS_FINISHED = "isFinished";
    public static final String TICKET_TYPE = "ticketType";
    public static final String TICKET_STATUS = "ticketStatus";
    public static final String TICKET_TEMPLATE = "ticketTemplate";
    public static final String EXECUTORS = "executors";

    public Specification<Ticket> getTicketByListOfAuthorsSpec(List<UUID> listAuthorId) {
        return (root, query, criteriaBuilder) -> root.get(AUTHOR).<UUID> get("id").in(listAuthorId);
    }

    public Specification<Ticket> getTicketByNumberSpec(Long number) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get(NUMBER), number);
    }

    public Specification<Ticket> getTicketByCreatedAtSpec(Long createdAt, String comparison) {
        switch (comparison) {
            case ">=" -> {
                return (root, query, criteriaBuilder) -> criteriaBuilder.ge(root.get(CREATED_AT), createdAt);
            }
            case ">" -> {
                return (root, query, criteriaBuilder) -> criteriaBuilder.gt(root.get(CREATED_AT), createdAt);
            }
            case "<=" -> {
                return (root, query, criteriaBuilder) -> criteriaBuilder.le(root.get(CREATED_AT), createdAt);
            }
            case "<" -> {
                return (root, query, criteriaBuilder) -> criteriaBuilder.lt(root.get(CREATED_AT), createdAt);
            }
            default -> {
                return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get(CREATED_AT), createdAt);
            }
        }
    }

    public Specification<Ticket> getTicketBySubjectSpec(String subject) {
        return (root, query, criteriaBuilder) -> criteriaBuilder
                .like(criteriaBuilder.lower(root.get(SUBJECT)), "%" + subject.toLowerCase() + "%");
    }

    public Specification<Ticket> getTicketByDescriptionSpec(String description) {
        return (root, query, criteriaBuilder) -> criteriaBuilder
                .like(criteriaBuilder.lower(root.get(DESCRIPTION)), "%" + description.toLowerCase() + "%");
    }

    public Specification<Ticket> getTicketByDeadlineSpec(Long deadline, String comparison) {
        switch (comparison) {
            case ">=" -> {
                return (root, query, criteriaBuilder) -> criteriaBuilder.ge(root.get(CREATED_AT), deadline);
            }
            case ">" -> {
                return (root, query, criteriaBuilder) -> criteriaBuilder.gt(root.get(CREATED_AT), deadline);
            }
            case "<=" -> {
                return (root, query, criteriaBuilder) -> criteriaBuilder.le(root.get(CREATED_AT), deadline);
            }
            case "<" -> {
                return (root, query, criteriaBuilder) -> criteriaBuilder.lt(root.get(CREATED_AT), deadline);
            }
            default -> {
                if (deadline == null) {
                    return (root, query, criteriaBuilder) -> criteriaBuilder.isNull(root.get(CREATED_AT));
                }
                return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get(CREATED_AT), deadline);
            }
        }
    }

    public Specification<Ticket> getTicketByIsFinishedSpec(Boolean isFinished) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get(IS_FINISHED), isFinished);
    }

    public Specification<Ticket> getTicketByListOfTicketTypeSpec(List<UUID> listTicketTypeId) {
        return (root, query, criteriaBuilder) -> root.get(TICKET_TYPE).<UUID> get("id").in(listTicketTypeId);
    }

    public Specification<Ticket> getTicketByListOfTicketStatusSpec(List<UUID> listTicketStatusId) {
        return (root, query, criteriaBuilder) -> root.get(TICKET_STATUS).<UUID> get("id").in(listTicketStatusId);
    }

    public Specification<Ticket> getTicketByListOfTicketTemplateSpec(List<UUID> listTicketTemplateId) {
        return (root, query, criteriaBuilder) -> root.get(TICKET_TEMPLATE).<UUID> get("id").in(listTicketTemplateId);
    }

    public Specification<Ticket> getTicketByAnyInListOfTicketExecutorsSpec(List<UUID> listTicketExecutorId) {
        return (root, query, criteriaBuilder) -> {
            query.distinct(true);
            ListJoin<Ticket, User> executors = root.joinList("executors");
            return executors.get("id").in(listTicketExecutorId);
        };
    }

    public Specification<Ticket> getTicketByAllInListOfTicketExecutorsSpec(List<UUID> listTicketExecutorId) {
        return (root, query, criteriaBuilder) -> {
            Subquery<Long> subQuery = query.subquery(Long.class);
            Root<Ticket> subRoot = subQuery.from(Ticket.class);

            ListJoin<Ticket, User> executors = subRoot.joinList("executors");

            Predicate ticketExecutorsInListOfExecutorsPredicate = executors.get("id").in(listTicketExecutorId);
            Predicate ticketsIdIsEqualsPredicate = criteriaBuilder.equal(root.get("id"), subRoot.get("id"));

            subQuery.select(criteriaBuilder.count(subRoot.get("id")));
            subQuery.where(ticketExecutorsInListOfExecutorsPredicate, ticketsIdIsEqualsPredicate);

            Long size = (long) listTicketExecutorId.size();

            return criteriaBuilder.equal(subQuery, size);
        };
    }

    public Specification<Ticket> getTicketByAllNotInListOfTicketExecutorsSpec(List<UUID> listTicketExecutorId) {
        return (root, query, criteriaBuilder) -> {
            Subquery<Long> subQuery = query.subquery(Long.class);
            Root<Ticket> subRoot = subQuery.from(Ticket.class);

            ListJoin<Ticket, User> executors = subRoot.joinList("executors");

            Predicate ticketExecutorsInListOfExecutorsPredicate = executors.get("id").in(listTicketExecutorId);
            Predicate ticketsIdIsEqualsPredicate = criteriaBuilder.equal(root.get("id"), subRoot.get("id"));

            subQuery.select(criteriaBuilder.count(subRoot.get("id")));
            subQuery.where(ticketExecutorsInListOfExecutorsPredicate, ticketsIdIsEqualsPredicate);

            Long size = (long) listTicketExecutorId.size();

            return criteriaBuilder.lessThan(subQuery, size);
        };
    }

    public Specification<Ticket> getTicketByNotAnyInListOfTicketExecutorsSpec(List<UUID> listTicketExecutorId) {
        return (root, query, criteriaBuilder) -> {
            Subquery<Long> subQuery = query.subquery(Long.class);
            Root<Ticket> subRoot = subQuery.from(Ticket.class);

            ListJoin<Ticket, User> executors = subRoot.joinList("executors");

            Predicate ticketExecutorsInListOfExecutorsPredicate = executors.get("id").in(listTicketExecutorId);
            Predicate ticketsIdIsEqualsPredicate = criteriaBuilder.equal(root.get("id"), subRoot.get("id"));

            subQuery.select(criteriaBuilder.count(subRoot.get("id")));
            subQuery.where(ticketExecutorsInListOfExecutorsPredicate, ticketsIdIsEqualsPredicate);

            return criteriaBuilder.equal(subQuery, 0);
        };
    }

    public Specification<Ticket> getTicketIsEqualListOfTicketExecutorsSpec(List<UUID> listTicketExecutorId) {
        return (root, query, criteriaBuilder) -> {
            Long size = (long) listTicketExecutorId.size();

            Subquery<Long> subQueryAllInList = query.subquery(Long.class);
            Root<Ticket> subRootAllInList = subQueryAllInList.from(Ticket.class);

            ListJoin<Ticket, User> executors = subRootAllInList.joinList("executors");

            Predicate ticketExecutorsInListOfExecutorsPredicate = executors.get("id").in(listTicketExecutorId);
            Predicate ticketsIdIsEqualsPredicate = criteriaBuilder.equal(root.get("id"), subRootAllInList.get("id"));

            subQueryAllInList.select(criteriaBuilder.count(subRootAllInList.get("id")));
            subQueryAllInList.where(ticketExecutorsInListOfExecutorsPredicate, ticketsIdIsEqualsPredicate);

            var allInList = criteriaBuilder.equal(subQueryAllInList, size);

            Subquery<Long> subQuerySizeOfList = query.subquery(Long.class);
            Root<Ticket> subRootSizeOfList = subQuerySizeOfList.from(Ticket.class);

            ListJoin<Ticket, User> executors2 = subRootSizeOfList.joinList("executors");

            Predicate ticketsIdIsEqualsPredicate2 = criteriaBuilder.equal(root.get("id"), subRootSizeOfList.get("id"));

            subQuerySizeOfList.select(criteriaBuilder.count(subRootSizeOfList.get("id")));
            subQuerySizeOfList.where(ticketsIdIsEqualsPredicate2);

            var sizeOfList = criteriaBuilder.equal(subQuerySizeOfList, size);


            return criteriaBuilder.and(allInList, sizeOfList);
        };
    }

    public Specification<Ticket> getTicketIsNotEqualListOfTicketExecutorsSpec(List<UUID> listTicketExecutorId) {
        return (root, query, criteriaBuilder) -> {
            Long size = (long) listTicketExecutorId.size();

            Subquery<Long> subQueryAllInList = query.subquery(Long.class);
            Root<Ticket> subRootAllInList = subQueryAllInList.from(Ticket.class);

            ListJoin<Ticket, User> executors = subRootAllInList.joinList("executors");

            Predicate ticketExecutorsInListOfExecutorsPredicate = executors.get("id").in(listTicketExecutorId);
            Predicate ticketsIdIsEqualsPredicate = criteriaBuilder.equal(root.get("id"), subRootAllInList.get("id"));

            subQueryAllInList.select(criteriaBuilder.count(subRootAllInList.get("id")));
            subQueryAllInList.where(ticketExecutorsInListOfExecutorsPredicate, ticketsIdIsEqualsPredicate);

            var allInList = criteriaBuilder.equal(subQueryAllInList, size);

            Subquery<Long> subQuerySizeOfList = query.subquery(Long.class);
            Root<Ticket> subRootSizeOfList = subQuerySizeOfList.from(Ticket.class);

            ListJoin<Ticket, User> executors2 = subRootSizeOfList.joinList("executors");

            Predicate ticketsIdIsEqualsPredicate2 = criteriaBuilder.equal(root.get("id"), subRootSizeOfList.get("id"));

            subQuerySizeOfList.select(criteriaBuilder.count(subRootSizeOfList.get("id")));
            subQuerySizeOfList.where(ticketsIdIsEqualsPredicate2);

            var sizeOfList = criteriaBuilder.equal(subQuerySizeOfList, size);

            return criteriaBuilder.and(allInList, sizeOfList).not();
        };
    }

    public Specification<Ticket> getTicketIsEmptyListOfTicketExecutorsSpec() {
        return (root, query, criteriaBuilder) -> {
            Subquery<Long> subQuerySizeOfList = query.subquery(Long.class);
            Root<Ticket> subRootSizeOfList = subQuerySizeOfList.from(Ticket.class);

            ListJoin<Ticket, User> executors = subRootSizeOfList.joinList("executors");

            Predicate ticketsIdIsEqualsPredicate = criteriaBuilder.equal(root.get("id"), subRootSizeOfList.get("id"));

            subQuerySizeOfList.select(criteriaBuilder.count(subRootSizeOfList.get("id")));
            subQuerySizeOfList.where(ticketsIdIsEqualsPredicate);

            return criteriaBuilder.equal(subQuerySizeOfList, 0);
        };
    }


    public Specification<Ticket> getTicketIsNotEmptyListOfTicketExecutorsSpec() {
        return (root, query, criteriaBuilder) -> {
            Subquery<Long> subQuerySizeOfList = query.subquery(Long.class);
            Root<Ticket> subRootSizeOfList = subQuerySizeOfList.from(Ticket.class);

            ListJoin<Ticket, User> executors = subRootSizeOfList.joinList("executors");

            Predicate ticketsIdIsEqualsPredicate = criteriaBuilder.equal(root.get("id"), subRootSizeOfList.get("id"));

            subQuerySizeOfList.select(criteriaBuilder.count(subRootSizeOfList.get("id")));
            subQuerySizeOfList.where(ticketsIdIsEqualsPredicate);

            return criteriaBuilder.notEqual(subQuerySizeOfList, 0);
        };
    }
}
