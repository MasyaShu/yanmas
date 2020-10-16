package ru.itterminal.botdesk.aau.model.spec;

import java.util.List;
import java.util.UUID;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.commons.model.spec.GeneralSpec;

@Component
public class UserSpec extends GeneralSpec<User> {

    public Specification<User> getUserByEmailSpec(String email) {
        return new Specification<User>() {
            @Override
            public Predicate toPredicate(Root<User> root,
                    CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) {
                Predicate predicate = criteriaBuilder.like(root.get("email"), "%" + email + "%");
                return predicate;
            }
        };
    }

    public Specification<User> getUserByFirstNameSpec(String firstName) {
        return new Specification<User>() {
            @Override
            public Predicate toPredicate(Root<User> root,
                    CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) {
                Predicate predicate = criteriaBuilder.like(root.get("first_name"), "%" + firstName + "%");
                return predicate;
            }
        };
    }

    public Specification<User> getUserBySecondNameSpec(String secondName) {
        return new Specification<User>() {
            @Override
            public Predicate toPredicate(Root<User> root,
                    CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) {
                Predicate predicate = criteriaBuilder.like(root.get("second_name"), "%" + secondName + "%");
                return predicate;
            }
        };
    }

    public Specification<User> getUserByPhoneSpec(String phone) {
        return new Specification<User>() {
            @Override
            public Predicate toPredicate(Root<User> root,
                    CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) {
                Predicate predicate = criteriaBuilder.like(root.get("phone"), "%" + phone + "%");
                return predicate;
            }
        };
    }

    public Specification<User> getUserByCommentSpec(String comment) {
        return new Specification<User>() {
            @Override
            public Predicate toPredicate(Root<User> root,
                    CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) {
                Predicate predicate = criteriaBuilder.like(root.get("comment"), "%" + comment + "%");
                return predicate;
            }
        };
    }

    public Specification<User> getUserByIsArchivedSpec(boolean isArchived) {
        return new Specification<User>() {
            @Override
            public Predicate toPredicate(Root<User> root,
                    CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) {
                Predicate predicate = criteriaBuilder.equal(root.get("is_archived"), isArchived);
                return predicate;
            }
        };
    }

    public Specification<User> getUserByAccountSpec(UUID accountId) {
        return new Specification<User>() {
            @Override
            public Predicate toPredicate(Root<User> root,
                    CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) {
                Join<User, Account> userJoin = root.join("account");
                Predicate predicate = criteriaBuilder.equal(userJoin.<UUID> get("id"), accountId);
                return predicate;
            }
        };
    }

    public Specification<User> getUserByListOfGroupsSpec(List<UUID> listGroupId) {
        return new Specification<User>() {
            @Override
            public Predicate toPredicate(Root<User> root,
                    CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) {
                Join<User, Group> userJoin = root.join("ownGroup");
                Predicate returnedPredicate = criteriaBuilder.equal(userJoin.<UUID> get("id"), listGroupId.get(0));
                for (int i = 1; i < listGroupId.size() ; i++) {
                    Predicate predicate = criteriaBuilder.equal(userJoin.<UUID> get("id"), listGroupId.get(i));
                    returnedPredicate = criteriaBuilder.or(returnedPredicate, predicate);
                }
                return returnedPredicate;
            }
        };
    }

    public Specification<User> getUserByListOfRolesSpec(List<UUID> listRoleId) {
        return new Specification<User>() {
            @Override
            public Predicate toPredicate(Root<User> root,
                    CriteriaQuery<?> query, CriteriaBuilder criteriaBuilder) {
                Join<User, Role> userJoin = root.join("role");
                Predicate returnedPredicate = criteriaBuilder.equal(userJoin.<UUID> get("id"), listRoleId.get(0));
                for (int i = 1; i < listRoleId.size() ; i++) {
                    Predicate predicate = criteriaBuilder.equal(userJoin.<UUID> get("id"), listRoleId.get(i));
                    returnedPredicate = criteriaBuilder.or(returnedPredicate, predicate);
                }
                return returnedPredicate;
            }
        };
    }
}
