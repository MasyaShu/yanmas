package ru.itterminal.botdesk.aau.model.spec;

import java.util.UUID;

import javax.persistence.criteria.Predicate;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import lombok.val;
import ru.itterminal.botdesk.commons.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.commons.model.spec.BaseSpec;

@Component
public class GroupSpec implements BaseSpec<Group, Account> {

    private static final String COMMENT = "comment";
    private static final String NAME = "name";
    private static final String IS_DEPRECATED = "isDeprecated";
    private static final String IS_INNER = "isInner";
    private static final String ID = "id";

    public Specification<Group> getGroupByNameSpec(String name) {
        return (root, query, criteriaBuilder) -> criteriaBuilder
                .like(criteriaBuilder.lower(root.get(NAME)), "%" + name.toLowerCase() + "%");
    }

    @SuppressWarnings("DuplicatedCode")
    public Specification<Group> getGroupByCommentSpec(String comment) {
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

    public Specification<Group> getGroupByIsDeprecatedSpec(Boolean isDeprecated) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get(IS_DEPRECATED), isDeprecated);
    }

    public Specification<Group> getGroupByIsInnerSpec(Boolean isInner) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get(IS_INNER), isInner);
    }

    public Specification<Group> getGroupByGroupSpec(UUID groupId) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get(ID), groupId);
    }
}
