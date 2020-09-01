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

    @Column(nullable = false)
    private String first_name;

    @Column
    private String second_name;

    @Column(nullable = false)
    private String password;

    @Column
    private String phone;

    @Column(nullable = false)
    private String comment;

    @Column(nullable = false)
    private String language;

    @Column(nullable = false)
    private String time_zone;

    @Column(nullable = false)
    private String email_verification_token;

    @Column(nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean email_verification_status;

    @Column
    private String password_reset_token;

    @Column(nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean is_archived;

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
                Objects.equals(first_name, user.first_name) &&
                Objects.equals(second_name, user.second_name) &&
                Objects.equals(password, user.password) &&
                Objects.equals(phone, user.phone) &&
                Objects.equals(comment, user.comment) &&
                Objects.equals(language, user.language) &&
                Objects.equals(time_zone, user.time_zone) &&
                Objects.equals(email_verification_token, user.email_verification_token) &&
                Objects.equals(email_verification_status, user.email_verification_status) &&
                Objects.equals(password_reset_token, user.password_reset_token) &&
                Objects.equals(is_archived, user.is_archived) &&
                Objects.equals(account, user.account) &&
                Objects.equals(getId(), user.getId()) &&
                Objects.equals(getVersion(), user.getVersion()) &&
                Objects.equals(getDeleted(), user.getDeleted());
    }

    @Override
    public int hashCode() {
        return Objects.hash(email, first_name, second_name, password, phone, comment, language, time_zone,
                email_verification_token, email_verification_status, password_reset_token, is_archived, account);
    }
}
