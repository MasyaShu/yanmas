package ru.itterminal.botdesk.aau.model.spec;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.commons.model.spec.BaseSpec;

import java.util.UUID;

@Component
public class GroupSpec implements BaseSpec<Group, Account> {

    public Specification<Group> getGroupByNameSpec(String name) {
        return (Specification<Group>) (root, query, criteriaBuilder) -> criteriaBuilder.like(criteriaBuilder.lower(root.get("name")), "%" + name.toLowerCase() + "%");
    }

    public Specification<Group> getGroupByCommentSpec(String comment) {
        return (Specification<Group>) (root, query, criteriaBuilder) -> criteriaBuilder.like(criteriaBuilder.lower(root.get("comment")), "%" + comment.toLowerCase() + "%");
    }

    public Specification<Group> getGroupByIsDeprecatedSpec(Boolean isDeprecated) {
        return (Specification<Group>) (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("isDeprecated"), isDeprecated);
    }

    public Specification<Group> getGroupByIsInnerSpec(Boolean isInner) {
        return (Specification<Group>) (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("isInner"), isInner);
    }


    public Specification<Group> getGroupByGroupSpec(UUID groupId) {
        return (Specification<Group>) (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("id"), groupId);
    }
}
