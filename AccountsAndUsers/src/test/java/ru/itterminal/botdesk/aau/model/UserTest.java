package ru.itterminal.botdesk.aau.model;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;

import ru.itterminal.botdesk.aau.model.test.UserTestHelper;

@TestInstance(PER_CLASS)
class UserTest {

    User user_1;
    User user_2;
    private final UUID id = UUID.randomUUID();
    private final Role admin = new Role(Roles.ADMIN.toString(), Roles.ADMIN.getWeight());
    private final UserTestHelper userTestHelper = new UserTestHelper();

    @SuppressWarnings("deprecation")
    @BeforeEach
    void setUp() {
        user_1 = User
                .builder()
                .account(null)
                .comment("comment")
                .email("email")
                .emailVerificationStatus(true)
                .emailVerificationToken("email_verification_token")
                .name("name")
                .isArchived(true)
                .password("12345")
                .passwordResetToken(null)
                .phone("7906558585")
                .role(admin)
                .build();
        user_1.setId(id);
        user_1.setVersion(0);
        user_1.setDeleted(false);

        user_2 = User
                .builder()
                .account(null)
                .comment("comment")
                .email("email")
                .emailVerificationStatus(true)
                .emailVerificationToken("email_verification_token")
                .name("name")
                .isArchived(true)
                .password("12345")
                .passwordResetToken(null)
                .phone("7906558585")
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
    void equals_shouldGetTrue_whenRolesDifferentObjectsBotEqualsValues() {
        Role admin = new Role(Roles.ADMIN.toString(), Roles.ADMIN.getWeight());
        user_1.setRole(admin);
        assertEquals(user_1, user_2);
    }

    @Test
    void generateDisplayName_shouldGetDisplayNameEqualEmail_whenNameIsNull () {
        User user = userTestHelper.getPredefinedValidEntityList().get(2);
        user.generateDisplayName();
        String expectedDisplayName = user.getEmail();
        String actualDisplayName = user.getDisplayName();
        assertEquals(expectedDisplayName, actualDisplayName);
    }

    @Test
    void generateDisplayName_shouldGetDisplayNameEqualName_whenNameIsNotNull () {
        User user = userTestHelper.getPredefinedValidEntityList().get(4);
        user.generateDisplayName();
        String expectedDisplayName = user.getName();
        String actualDisplayName = user.getDisplayName();
        assertEquals(expectedDisplayName, actualDisplayName);
    }

}