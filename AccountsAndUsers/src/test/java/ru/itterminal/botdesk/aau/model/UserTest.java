package ru.itterminal.botdesk.aau.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;

@TestInstance(PER_CLASS)
class UserTest {

    User user_1;
    User user_2;
    UUID id = UUID.randomUUID();

    Role admin = new Role(Roles.ADMIN.toString());
    Role author = new Role(Roles.AUTHOR.toString());

    @BeforeEach
    private void setUp() {
        user_1 = new User().builder()
                .account(null)
                .comment("comment")
                .email("email")
                .emailVerificationStatus(true)
                .emailVerificationToken("email_verification_token")
                .firstName("first_name")
                .isArchived(true)
                .language("ru")
                .password("12345")
                .passwordResetToken(null)
                .phone("7906558585")
                .secondName("second_name")
                .role(admin)
                .build();
        user_1.setId(id);
        user_1.setVersion(0);
        user_1.setDeleted(false);

        user_2 = new User().builder()
                .account(null)
                .comment("comment")
                .email("email")
                .emailVerificationStatus(true)
                .emailVerificationToken("email_verification_token")
                .firstName("first_name")
                .isArchived(true)
                .language("ru")
                .password("12345")
                .passwordResetToken(null)
                .phone("7906558585")
                .secondName("second_name")
                .role(admin)
                .build();
        user_2.setId(id);
        user_2.setVersion(0);
        user_2.setDeleted(false);
    }

    @Test
    void equals_shouldGetTrue_whenAllFieldsAreEquals() {
        assertEquals(user_1, user_2);
    }

    @Test
    void equals_shouldGetFalse_whenOneFieldAreNotEquals() {
        user_1.setComment("");
        assertNotEquals(user_1, user_2);
    }

    @Test
    void equals_shouldGetTrue_whenBothObjectsIsNew() {
        User user_1 = new User();
        User user_2 = new User();
        assertEquals(user_1, user_2);
    }

    @Test
    void equals_shouldGetFalse_whenRolesNotEquals() {
        user_1.setRole(null);
        assertNotEquals(user_1, user_2);
    }

    @Test
    void equals_shouldGetTrue_whenRolesDifferntObjectsBotEqualsValues() {
        Role admin = new Role(Roles.ADMIN.toString());
        user_1.setRole(admin);
        assertEquals(user_1, user_2);
    }
}