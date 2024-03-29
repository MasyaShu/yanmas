package ru.itterminal.botdesk.files.service.validator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createExpectedLogicalValidationException;
import static ru.itterminal.botdesk.files.service.validator.FileOperationValidator.CREATED_AT;
import static ru.itterminal.botdesk.files.service.validator.FileOperationValidator.CREATED_AT_NULL;
import static ru.itterminal.botdesk.files.service.validator.FileOperationValidator.CREATED_AT_ZERO;
import static ru.itterminal.botdesk.files.service.validator.FileOperationValidator.FILE_NAME;
import static ru.itterminal.botdesk.files.service.validator.FileOperationValidator.FILE_NAME_EMPTY;
import static ru.itterminal.botdesk.files.service.validator.FileOperationValidator.FILE_NAME_NULL;

import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.files.model.File;

@SpringJUnitConfig(value = {FileOperationValidator.class})
class FileOperationValidatorTest {

    @Autowired
    FileOperationValidator validator;

    @Test
    void beforeCreate_shouldGetTrue_whenSizeOfFileLessThan25Mb() {
        File file = File.builder()
                .size(1000)
                .fileName(FILE_NAME)
                .createdAt(1000L)
                .account(Account.builder().build())
                .entityId(UUID.randomUUID())
                .build();
        assertTrue(validator.beforeCreate(file));
    }

    @Test
    void beforeCreate_shouldGetLogicalValidationException_whenFilenameIsNull() {
        LogicalValidationException expectedException = createExpectedLogicalValidationException(FILE_NAME, FILE_NAME_NULL);
        File file = File.builder()
                .size(1000)
                .fileName(null)
                .build();
        LogicalValidationException actualException = assertThrows(
                LogicalValidationException.class,
                () -> validator.beforeCreate(file)
        );
        assertEquals(
                expectedException.getFieldErrors().get(FILE_NAME).get(0),
                actualException.getFieldErrors().get(FILE_NAME).get(0)
        );
    }

    @Test
    void beforeCreate_shouldGetLogicalValidationException_whenFilenameIsEmpty() {
        LogicalValidationException expectedException = createExpectedLogicalValidationException(FILE_NAME, FILE_NAME_EMPTY);
        File file = File.builder()
                .size(1000)
                .fileName("")
                .build();
        LogicalValidationException actualException = assertThrows(
                LogicalValidationException.class,
                () -> validator.beforeCreate(file)
        );
        assertEquals(
                expectedException.getFieldErrors().get(FILE_NAME).get(0),
                actualException.getFieldErrors().get(FILE_NAME).get(0)
        );
    }

    @Test
    void beforeCreate_shouldGetLogicalValidationException_whenCreatedAtIsNull() {
        LogicalValidationException expectedException = createExpectedLogicalValidationException(CREATED_AT, CREATED_AT_NULL);
        File file = File.builder()
                .size(1000)
                .fileName(FILE_NAME)
                .createdAt(null)
                .build();
        LogicalValidationException actualException = assertThrows(
                LogicalValidationException.class,
                () -> validator.beforeCreate(file)
        );
        assertEquals(
                expectedException.getFieldErrors().get(CREATED_AT).get(0),
                actualException.getFieldErrors().get(CREATED_AT).get(0)
        );
    }

    @Test
    void beforeCreate_shouldGetLogicalValidationException_whenCreatedAtIsZero() {
        LogicalValidationException expectedException = createExpectedLogicalValidationException(CREATED_AT, CREATED_AT_ZERO);
        File file = File.builder()
                .size(1000)
                .fileName(FILE_NAME)
                .createdAt(0L)
                .build();
        LogicalValidationException actualException = assertThrows(
                LogicalValidationException.class,
                () -> validator.beforeCreate(file)
        );
        assertEquals(
                expectedException.getFieldErrors().get(CREATED_AT).get(0),
                actualException.getFieldErrors().get(CREATED_AT).get(0)
        );
    }
}