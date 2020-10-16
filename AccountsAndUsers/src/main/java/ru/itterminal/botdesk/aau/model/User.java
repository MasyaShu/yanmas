package ru.itterminal.botdesk.aau.model;

import lombok.*;
import ru.itterminal.botdesk.commons.model.BaseEntity;

import javax.persistence.*;
import java.util.Objects;
import java.util.Set;

@Entity
@Table(name = "users")
@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class User extends BaseEntity {

    @Column(nullable = false, unique = true, length = 128)
    private String email;

    @Column(name = "first_name", length = 20)
    private String firstName;

    @Column(name = "second_name", length = 30)
    private String secondName;

    @Column(nullable = false)
    private String password;

    @Column (length = 30)
    private String phone;

    @Column
    private String comment;

    @Column (length = 2)
    private String language;

    @Column(name = "email_verification_token", length = 128)
    private String emailVerificationToken;

    @Column(name = "email_verification_status", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean emailVerificationStatus;

    @Column(name = "password_reset_token", length = 128)
    private String passwordResetToken;

    @Column(name = "is_archived", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isArchived;

    @ManyToOne
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @ManyToOne
    @JoinColumn(name = "own_group_id", nullable = false)
    private Group ownGroup;

    @ManyToOne
    @JoinColumn(name = "role_id", nullable = false)
    private Role role;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof User)) {
            return false;
        }
        User user = (User) o;
        return Objects.equals(email, user.email) &&
                Objects.equals(firstName, user.firstName) &&
                Objects.equals(secondName, user.secondName) &&
                Objects.equals(password, user.password) &&
                Objects.equals(phone, user.phone) &&
                Objects.equals(comment, user.comment) &&
                Objects.equals(language, user.language) &&
                Objects.equals(emailVerificationToken, user.emailVerificationToken) &&
                Objects.equals(emailVerificationStatus, user.emailVerificationStatus) &&
                Objects.equals(passwordResetToken, user.passwordResetToken) &&
                Objects.equals(isArchived, user.isArchived) &&
                Objects.equals(account, user.account) &&
                Objects.equals(role, user.role) &&
                Objects.equals(getId(), user.getId()) &&
                Objects.equals(getOutId(), user.getOutId()) &&
                Objects.equals(getVersion(), user.getVersion()) &&
                Objects.equals(getDeleted(), user.getDeleted());
    }

    @Override
    public int hashCode() {
        return Objects.hash(email, firstName, secondName, password, phone, comment, language,
                emailVerificationToken, emailVerificationStatus, passwordResetToken, isArchived, account, role);
    }
}
