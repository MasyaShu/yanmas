package ru.itterminal.botdesk.aau.service.validator;

import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.projection.UserUniqueFields;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.commons.exception.error.ValidationError;

@ExtendWith(SpringExtension.class)
class UserOperationValidatorTest {

    @Mock
    private UserServiceImpl service;

    @Mock
    private UserUniqueFields userUniqueFields;

    BCryptPasswordEncoder encoder = new BCryptPasswordEncoder();

    @InjectMocks
    private UserOperationValidator validator = new UserOperationValidator(service, encoder);


    private static final String EXIST_EMAIL = "mail@mal.ru";
    private static User user;
    private static LogicalValidationException logicalValidationException;
    private static Map<String, List<ValidationError>> errors = new HashMap<>();

    @BeforeAll
    static void setUp() {
        user = new User().builder()
                .email(EXIST_EMAIL)
                .build();
        errors.put("email", singletonList(new ValidationError("not unique", "email is occupied")));
        logicalValidationException = new LogicalValidationException("Validation failed", errors);
    }

    @Test
    public void checkUniqueness_shouldGetTrue_whenPassedDataIsUnique() {
        when(service.findByUniqueFields(any())).thenReturn(Collections.emptyList());
        assertTrue(validator.checkUniqueness(new User()));
    }

    @Test
    public void checkUniqueness_shouldGetFalse_whenPassedDataNotUnique() {
        when(service.findByUniqueFields(any())).thenReturn(List.of(userUniqueFields));
        when(userUniqueFields.getEmail()).thenReturn(EXIST_EMAIL);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.checkUniqueness(user));
        assertEquals(logicalValidationException.getFieldErrors().get("email").get(0),
                thrown.getFieldErrors().get("email").get(0));
    }

    @Test
    public void encoderPassword() {
        String encodedPassword = encoder.encode("12345");
        System.out.println("encodedPassword: " + encodedPassword);
        assertTrue(encoder.matches("12345", encodedPassword));
    }

}