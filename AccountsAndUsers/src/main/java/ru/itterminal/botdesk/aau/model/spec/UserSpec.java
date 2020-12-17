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

    private static final String FIRST_NAME = "firstName";
    private static final String SECOND_NAME = "secondName";
    private static final String PHONE = "phone";
    private static final String COMMENT = "comment";
    private static final String EMPTY_STRING = "";

    public Specification<User> getUserByEmailSpec(String email) {
        return (root, query, criteriaBuilder) -> criteriaBuilder.like(criteriaBuilder.lower(root.get("email")),
                "%" + email.toLowerCase() + "%");
    }

    @SuppressWarnings("DuplicatedCode")
    public Specification<User> getUserByFirstNameSpec(String firstName) {
        return (root, query, criteriaBuilder) -> {
            val objectPathFirstName = root.get(FIRST_NAME);
            if (firstName.isEmpty()) {
                Predicate predicateForNull =  criteriaBuilder.isNull(objectPathFirstName);
                Predicate predicateForEmpty =  criteriaBuilder.equal(objectPathFirstName, EMPTY_STRING);
                return criteriaBuilder.or(predicateForEmpty, predicateForNull);
            }
            return criteriaBuilder.like(criteriaBuilder.lower(root.get(FIRST_NAME)),
                    "%" + firstName.toLowerCase() + "%");
        };
    }

    @SuppressWarnings("DuplicatedCode")
    public Specification<User> getUserBySecondNameSpec(String secondName) {
        return (root, query, criteriaBuilder) -> {
            val objectPathSecondName = root.get(SECOND_NAME);
            if (secondName.isEmpty()) {
                Predicate predicateForNull =  criteriaBuilder.isNull(objectPathSecondName);
                Predicate predicateForEmpty =  criteriaBuilder.equal(objectPathSecondName, EMPTY_STRING);
                return criteriaBuilder.or(predicateForEmpty, predicateForNull);
            }
            return criteriaBuilder.like(criteriaBuilder.lower(root.get(SECOND_NAME)),
                    "%" + secondName.toLowerCase() + "%");
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
