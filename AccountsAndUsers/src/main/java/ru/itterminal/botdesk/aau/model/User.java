package ru.itterminal.botdesk.aau.model;

import java.util.Objects;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.BaseEntity;

@Entity
@Table(name = "users")
@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class User extends BaseEntity {

    @Column(nullable = false, unique = true, length = 128)
    private String email;

    @Column(length = 128)
    private String name;

    @Column(nullable = false)
    private String password;

    @Column (length = 30)
    private String phone;

    @Column
    private String comment;

    @Column(name = "email_verification_token", length = 128)
    private String emailVerificationToken;

    @Column(name = "email_verification_status", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean emailVerificationStatus;

    @Column(name = "password_reset_token", length = 128)
    private String passwordResetToken;

    @Column(name = "is_archived", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isArchived;

    @ManyToOne
    @JoinColumn(name = "group_id", nullable = false)
    private Group group;

    @ManyToOne
    @JoinColumn(name = "role_id", nullable = false)
    private Role role;

    @ManyToOne
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

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
                Objects.equals(name, user.name) &&
                Objects.equals(password, user.password) &&
                Objects.equals(phone, user.phone) &&
                Objects.equals(comment, user.comment) &&
                Objects.equals(emailVerificationToken, user.emailVerificationToken) &&
                Objects.equals(emailVerificationStatus, user.emailVerificationStatus) &&
                Objects.equals(passwordResetToken, user.passwordResetToken) &&
                Objects.equals(isArchived, user.isArchived) &&
                Objects.equals(role, user.role) &&
                Objects.equals(account, user.account) &&
                Objects.equals(getId(), user.getId()) &&
                Objects.equals(getOutId(), user.getOutId()) &&
                Objects.equals(getVersion(), user.getVersion()) &&
                Objects.equals(getDeleted(), user.getDeleted());
    }

    @Override
    public int hashCode() {
        return Objects.hash(email, name, password, phone, comment,
                emailVerificationToken, emailVerificationStatus, passwordResetToken, isArchived, role, account,
                getId(), getOutId(), getVersion(), getDeleted());
    }

    @Override
    public void generateDisplayName() {
        if (name==null || name.isEmpty()) {
            setDisplayName(email);
            return;
        }
        setDisplayName(name);
    }
}
