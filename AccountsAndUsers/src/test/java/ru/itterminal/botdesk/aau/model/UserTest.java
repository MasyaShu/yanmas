package ru.itterminal.botdesk.aau.model;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;

@TestInstance(PER_CLASS)
class UserTest {

    User user_1;
    User user_2;
    UUID id;

    @BeforeEach
    private void setUp() {
        id = UUID.randomUUID();
        user_1 = new User().builder()
                .account(null)
                .comment("comment")
                .email("email")
                .email_verification_status(true)
                .email_verification_token("email_verification_token")
                .first_name("first_name")
                .is_archived(true)
                .language("ru")
                .password("12345")
                .password_reset_token(null)
                .phone("7906558585")
                .second_name("second_name")
                .time_zone("time_zone")
                .build();
        user_1.setId(id);
        user_1.setVersion(0);
        user_1.setDeleted(false);

        user_2 = new User().builder()
                .account(null)
                .comment("comment")
                .email("email")
                .email_verification_status(true)
                .email_verification_token("email_verification_token")
                .first_name("first_name")
                .is_archived(true)
                .language("ru")
                .password("12345")
                .password_reset_token(null)
                .phone("7906558585")
                .second_name("second_name")
                .time_zone("time_zone")
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
}