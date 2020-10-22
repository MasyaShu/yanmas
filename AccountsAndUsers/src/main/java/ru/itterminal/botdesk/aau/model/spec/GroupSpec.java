package ru.itterminal.botdesk.aau.model.spec;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.commons.model.spec.GeneralSpec;

import javax.persistence.criteria.*;
import java.util.UUID;

@Component
public class GroupSpec extends GeneralSpec<Group> {

    public Specification<Group> getGroupByNameSpec(String name) {
        return new Specification<Group>() {
            @Override
            public Predicate toPredicate(Root<Group> root,
                    CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) {
                Predicate predicate = criteriaBuilder.like(root.get("name"), "%" + name + "%");
                return predicate;
            }
        };
    }

    public Specification<Group> getGroupByCommentSpec(String comment) {
        return new Specification<Group>() {
            @Override
            public Predicate toPredicate(Root<Group> root,
                    CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) {
                Predicate predicate = criteriaBuilder.like(root.get("comment"), "%" + comment + "%");
                return predicate;
            }
        };
    }

    public Specification<Group> getGroupByIsDeprecatedSpec(Boolean isDeprecated) {
        return new Specification<Group>() {
            @Override
            public Predicate toPredicate(Root<Group> root,
                                         CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) {
                Predicate predicate = criteriaBuilder.equal(root.get("isDeprecated"), isDeprecated);
                return predicate;
            }
        };
    }

    public Specification<Group> getGroupByIsInnerSpec(Boolean isInner) {
        return new Specification<Group>() {
            @Override
            public Predicate toPredicate(Root<Group> root,
                                         CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) {
                Predicate predicate = criteriaBuilder.equal(root.get("isInner"), isInner);
                return predicate;
            }
        };
    }

    public Specification<Group> getGroupByAccountSpec(UUID accountId) {
        return new Specification<Group>() {
            @Override
            public Predicate toPredicate(Root<Group> root,
                    CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) {
                Join<Group, Account> groupJoin = root.join("account");
                Predicate predicate = criteriaBuilder.equal(groupJoin.<UUID> get("id"), accountId);
                return predicate;
            }
        };
    }
}
