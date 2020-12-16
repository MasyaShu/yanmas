package ru.itterminal.botdesk.files.service.validator;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static ru.itterminal.botdesk.commons.util.CommonMethodsForValidation.createExpectedLogicalValidationException;

import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.botdesk.commons.model.Account;
import ru.itterminal.botdesk.commons.exception.LogicalValidationException;
import ru.itterminal.botdesk.files.model.File;

@SpringJUnitConfig(value = {FileOperationValidator.class})
@TestPropertySource(properties = {"maxSizeOfFile=26214400"})
class FileOperationValidatorTest {

    @Autowired
    FileOperationValidator validator;

    private static final String SIZE_FILE = "Size of file";
    private static final String MAX_SIZE = "Size of file mustn't over 25Mb";
    private static final String FILE_NAME = "Filename";
    private static final String FILE_NAME_EMPTY = "Filename mustn't empty";
    private static final String SIZE_NULL = "Size of file mustn't null";
    private static final String FILE_NAME_NULL = "Filename mustn't null";
    private static final String CREATED_AT = "Created at";
    private static final String CREATED_AT_NULL = "Created at mustn't null";
    private static final String CREATED_AT_ZERO = "Created at mustn't zero";
    private static final String ACCOUNT = "Account";
    private static final String ACCOUNT_NULL = "Account mustn't null";
    private static final String ENTITY_ID = "EntityId";
    private static final String ENTITY_ID_NULL = "EntityId mustn't null";


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
    void beforeCreate_shouldGetLogicalValidationException_whenSizeOfFileMoreThan25Mb() {
        LogicalValidationException expectedException = createExpectedLogicalValidationException(SIZE_FILE, MAX_SIZE);
        File file = File.builder()
                .size(27214400)
                .build();
        LogicalValidationException actualException = assertThrows(
                LogicalValidationException.class,
                () -> validator.beforeCreate(file)
        );
        assertEquals(
                expectedException.getFieldErrors().get(SIZE_FILE).get(0),
                actualException.getFieldErrors().get(SIZE_FILE).get(0)
        );
    }

    @Test
    void beforeCreate_shouldGetLogicalValidationException_whenSizeIsNull() {
        LogicalValidationException expectedException = createExpectedLogicalValidationException(SIZE_FILE, SIZE_NULL);
        File file = File.builder()
                .size(null)
                .fileName(FILE_NAME)
                .build();
        LogicalValidationException actualException = assertThrows(
                LogicalValidationException.class,
                () -> validator.beforeCreate(file)
        );
        assertEquals(
                expectedException.getFieldErrors().get(SIZE_FILE).get(0),
                actualException.getFieldErrors().get(SIZE_FILE).get(0)
        );
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

    @Test
    void beforeCreate_shouldGetLogicalValidationException_whenAccountIsNull() {
        LogicalValidationException expectedException = createExpectedLogicalValidationException(ACCOUNT, ACCOUNT_NULL);
        File file = File.builder()
                .size(1000)
                .fileName(FILE_NAME)
                .createdAt(1000L)
                .account(null)
                .build();
        LogicalValidationException actualException = assertThrows(
                LogicalValidationException.class,
                () -> validator.beforeCreate(file)
        );
        assertEquals(
                expectedException.getFieldErrors().get(ACCOUNT).get(0),
                actualException.getFieldErrors().get(ACCOUNT).get(0)
        );
    }

    @Test
    void beforeCreate_shouldGetLogicalValidationException_whenEntityIdIsNull() {
        LogicalValidationException expectedException = createExpectedLogicalValidationException(ENTITY_ID, ENTITY_ID_NULL);
        File file = File.builder()
                .size(1000)
                .fileName(FILE_NAME)
                .createdAt(1000L)
                .account(Account.builder().build())
                .entityId(null)
                .build();
        LogicalValidationException actualException = assertThrows(
                LogicalValidationException.class,
                () -> validator.beforeCreate(file)
        );
        assertEquals(
                expectedException.getFieldErrors().get(ENTITY_ID).get(0),
                actualException.getFieldErrors().get(ENTITY_ID).get(0)
        );
    }
}