package ru.itterminal.botdesk.aau.model.spec;

import java.util.List;
import java.util.UUID;

import javax.persistence.criteria.Join;
import javax.persistence.criteria.Predicate;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.commons.model.spec.BaseSpec;

@Component
public class UserSpec implements BaseSpec<User, Account> {

    public Specification<User> getUserByEmailSpec(String email) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(criteriaBuilder.lower(root.get("email")),
                "%" + email.toLowerCase() + "%");
    }

    public Specification<User> getUserByFirstNameSpec(String firstName) {
        if (firstName == null) {
            return (root, query, criteriaBuilder) -> criteriaBuilder.isNull(root.get("firstName"));
        }
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(criteriaBuilder.lower(root.get("firstName")),
                "%" + firstName.toLowerCase() + "%");
    }

    public Specification<User> getUserBySecondNameSpec(String secondName) {
        if (secondName == null) {
            return (root, query, criteriaBuilder) -> criteriaBuilder.isNull(root.get("secondName"));
        }
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(criteriaBuilder.lower(root.get("secondName")),
                "%" + secondName.toLowerCase() + "%");
    }

    public Specification<User> getUserByPhoneSpec(String phone) {
        if (phone == null) {
            return (root, query, criteriaBuilder) -> criteriaBuilder.isNull(root.get("phone"));
        }
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(criteriaBuilder.lower(root.get("phone")),
                "%" + phone.toLowerCase() + "%");
    }

    public Specification<User> getUserByCommentSpec(String comment) {
        if (comment == null) {
            return (root, query, criteriaBuilder) -> criteriaBuilder.isNull(root.get("comment"));
        }
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(criteriaBuilder.lower(root.get("comment")),
                "%" + comment.toLowerCase() + "%");
    }

    public Specification<User> getUserByIsArchivedSpec(boolean isArchived) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("isArchived"), isArchived);
    }

    @SuppressWarnings("DuplicatedCode")
    public Specification<User> getUserByListOfGroupsSpec(List<UUID> listGroupId) {
        return (root, query, criteriaBuilder) -> {
            Join<User, Group> userJoin = root.join("ownGroup");
            Predicate returnedPredicate = criteriaBuilder.equal(userJoin.<UUID> get("id"), listGroupId.get(0));
            for (int i = 1; i < listGroupId.size(); i++) {
                Predicate predicate = criteriaBuilder.equal(userJoin.<UUID> get("id"), listGroupId.get(i));
                returnedPredicate = criteriaBuilder.or(returnedPredicate, predicate);
            }
            return returnedPredicate;
        };
    }

    @SuppressWarnings("DuplicatedCode")
    public Specification<User> getUserByListOfRolesSpec(List<UUID> listRoleId) {
        return (root, query, criteriaBuilder) -> {
            Join<User, Role> userJoin = root.join("role");
            Predicate returnedPredicate = criteriaBuilder.equal(userJoin.<UUID> get("id"), listRoleId.get(0));
            for (int i = 1; i < listRoleId.size(); i++) {
                Predicate predicate = criteriaBuilder.equal(userJoin.<UUID> get("id"), listRoleId.get(i));
                returnedPredicate = criteriaBuilder.or(returnedPredicate, predicate);
            }
            return returnedPredicate;
        };
    }
}
