package ru.itterminal.botdesk.aau.model;

import java.util.Objects;
import java.util.Set;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToMany;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import ru.itterminal.botdesk.commons.model.BaseEntity;

@Entity
@Table(name = "user")
@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class User extends BaseEntity {

    @Column(nullable = false, unique = true)
    private String email;

    @Column(name = "first_name")
    private String firstName;

    @Column(name = "second_name")
    private String secondName;

    //TODO Must save as encoded
    @Column(nullable = false)
    private String password;

    @Column
    private String phone;

    @Column
    private String comment;

    @Column
    private String language;

    @Column(name = "time_zone")
    private String timeZone;

    @Column(name = "email_verification_token")
    private String emailVerificationToken;

    @Column(name = "email_verification_status", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean emailVerificationStatus;

    @Column(name = "password_reset_token")
    private String passwordResetToken;

    @Column(name = "is_archived", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isArchived;

    @ManyToOne
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @ManyToMany(cascade = {CascadeType.PERSIST}, fetch = FetchType.EAGER)
    @JoinTable(name = "user_role",
            joinColumns = @JoinColumn(name = "user_id", referencedColumnName = "id"),
            inverseJoinColumns = @JoinColumn(name = "role_id", referencedColumnName = "id"))
    private Set<Role> roles;

    // TODO add consumer_id

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
                Objects.equals(timeZone, user.timeZone) &&
                Objects.equals(emailVerificationToken, user.emailVerificationToken) &&
                Objects.equals(emailVerificationStatus, user.emailVerificationStatus) &&
                Objects.equals(passwordResetToken, user.passwordResetToken) &&
                Objects.equals(isArchived, user.isArchived) &&
                Objects.equals(account, user.account) &&
                Objects.equals(roles, user.roles) &&
                Objects.equals(getId(), user.getId()) &&
                Objects.equals(getVersion(), user.getVersion()) &&
                Objects.equals(getDeleted(), user.getDeleted());
    }

    @Override
    public int hashCode() {
        return Objects.hash(email, firstName, secondName, password, phone, comment, language, timeZone,
                emailVerificationToken, emailVerificationStatus, passwordResetToken, isArchived, account, roles);
    }
}
