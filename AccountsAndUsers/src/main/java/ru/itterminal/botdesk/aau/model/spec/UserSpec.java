package ru.itterminal.botdesk.aau.model.spec;

import java.util.List;
import java.util.UUID;

import javax.persistence.criteria.Join;
import javax.persistence.criteria.Predicate;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import lombok.val;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.commons.model.spec.BaseSpec;

@Component
public class UserSpec implements BaseSpec<User, Account> {

    private static final String NAME = "name";
    private static final String PHONE = "phone";
    private static final String COMMENT = "comment";
    private static final String EMPTY_STRING = "";

    public Specification<User> getUserByEmailSpec(String email) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(criteriaBuilder.lower(root.get("email")),
                "%" + email.toLowerCase() + "%");
    }

    @SuppressWarnings("DuplicatedCode")
    public Specification<User> getUserByNameSpec(String name) {
        return (root, query, criteriaBuilder) -> {
            val objectPathname = root.get(NAME);
            if (name.isEmpty()) {
                Predicate predicateForNull =  criteriaBuilder.isNull(objectPathname);
                Predicate predicateForEmpty =  criteriaBuilder.equal(objectPathname, EMPTY_STRING);
                return criteriaBuilder.or(predicateForEmpty, predicateForNull);
            }
            return criteriaBuilder.like(criteriaBuilder.lower(root.get(NAME)),
                    "%" + name.toLowerCase() + "%");
        };
    }

    @SuppressWarnings("DuplicatedCode")
    public Specification<User> getUserByPhoneSpec(String phone) {
        return (root, query, criteriaBuilder) -> {
            val objectPathPhone = root.get(PHONE);
            if (phone.isEmpty()) {
                Predicate predicateForNull =  criteriaBuilder.isNull(objectPathPhone);
                Predicate predicateForEmpty =  criteriaBuilder.equal(objectPathPhone, EMPTY_STRING);
                return criteriaBuilder.or(predicateForEmpty, predicateForNull);
            }
            return criteriaBuilder.like(criteriaBuilder.lower(root.get(PHONE)),
                    "%" + phone.toLowerCase() + "%");
        };
    }

    @SuppressWarnings("DuplicatedCode")
    public Specification<User> getUserByCommentSpec(String comment) {
        return (root, query, criteriaBuilder) -> {
            val objectPathComment = root.get(COMMENT);
            if (comment.isEmpty()) {
                Predicate predicateForNull =  criteriaBuilder.isNull(objectPathComment);
                Predicate predicateForEmpty =  criteriaBuilder.equal(objectPathComment, EMPTY_STRING);
                return criteriaBuilder.or(predicateForEmpty, predicateForNull);
            }
            return criteriaBuilder.like(criteriaBuilder.lower(root.get(COMMENT)),
                    "%" + comment.toLowerCase() + "%");
        };
    }

    public Specification<User> getUserByIsArchivedSpec(boolean isArchived) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.equal(root.get("isArchived"), isArchived);
    }

    @SuppressWarnings("DuplicatedCode")
    public Specification<User> getUserByListOfGroupsSpec(List<UUID> listGroupId) {
        return (root, query, criteriaBuilder) -> {
            Join<User, Group> userJoin = root.join("group");
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
