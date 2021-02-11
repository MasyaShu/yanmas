package ru.itterminal.botdesk.aau.model;

import javax.persistence.*;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.BaseEntity;

@Entity
@Table(name = "users")
@Getter
@Setter
@SuperBuilder(toBuilder = true)
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class User extends BaseEntity {

    @Column(nullable = false, unique = true, length = 128)
    private String email;

    @Column(length = 128)
    private String name;

    @Column(nullable = false)
    private String password;

    @Column(length = 30)
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

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "group_id", nullable = false)
    private Group group;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "role_id", nullable = false)
    private Role role;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @Override
    public void generateDisplayName() {
        if (name == null || name.isEmpty()) {
            setDisplayName(email);
            return;
        }
        setDisplayName(name);
    }

    @PrePersist
    protected void onCreate() {
        setDeleted(false);
    }

}
