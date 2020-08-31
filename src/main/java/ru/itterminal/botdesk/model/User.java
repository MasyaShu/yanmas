package ru.itterminal.botdesk.model;

import javax.persistence.Column;
import javax.persistence.Entity;
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
    @Column(name = "name", nullable = false, unique = true)
    private String name;
    private String email;
    private String first_name;
    private String second_name;
    private String password;
    private String phone;
    private String comment;
    private String language;
    private String time_zone;
    private String email_verification_token;
    private String password_reset_token;
    private Boolean email_verification_status;
    private Boolean is_archived;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }

        User user = (User) o;

        if (name != null ? !name.equals(user.name) : user.name != null) {
            return false;
        }
        if (email != null ? !email.equals(user.email) : user.email != null) {
            return false;
        }
        if (first_name != null ? !first_name.equals(user.first_name) : user.first_name != null) {
            return false;
        }
        if (second_name != null ? !second_name.equals(user.second_name) : user.second_name != null) {
            return false;
        }
        if (password != null ? !password.equals(user.password) : user.password != null) {
            return false;
        }
        if (phone != null ? !phone.equals(user.phone) : user.phone != null) {
            return false;
        }
        if (comment != null ? !comment.equals(user.comment) : user.comment != null) {
            return false;
        }
        if (language != null ? !language.equals(user.language) : user.language != null) {
            return false;
        }
        if (time_zone != null ? !time_zone.equals(user.time_zone) : user.time_zone != null) {
            return false;
        }
        if (email_verification_token != null ? !email_verification_token.equals(user.email_verification_token)
                : user.email_verification_token != null) {
            return false;
        }
        if (password_reset_token != null ? !password_reset_token.equals(user.password_reset_token)
                : user.password_reset_token != null) {
            return false;
        }
        if (email_verification_status != null ? !email_verification_status.equals(user.email_verification_status)
                : user.email_verification_status != null) {
            return false;
        }
        return is_archived != null ? is_archived.equals(user.is_archived) : user.is_archived == null;
    }

    @Override
    public int hashCode() {
        int result = name != null ? name.hashCode() : 0;
        result = 31 * result + (email != null ? email.hashCode() : 0);
        result = 31 * result + (first_name != null ? first_name.hashCode() : 0);
        result = 31 * result + (second_name != null ? second_name.hashCode() : 0);
        result = 31 * result + (password != null ? password.hashCode() : 0);
        result = 31 * result + (phone != null ? phone.hashCode() : 0);
        result = 31 * result + (comment != null ? comment.hashCode() : 0);
        result = 31 * result + (language != null ? language.hashCode() : 0);
        result = 31 * result + (time_zone != null ? time_zone.hashCode() : 0);
        result = 31 * result + (email_verification_token != null ? email_verification_token.hashCode() : 0);
        result = 31 * result + (password_reset_token != null ? password_reset_token.hashCode() : 0);
        result = 31 * result + (email_verification_status != null ? email_verification_status.hashCode() : 0);
        result = 31 * result + (is_archived != null ? is_archived.hashCode() : 0);
        return result;
    }
}
