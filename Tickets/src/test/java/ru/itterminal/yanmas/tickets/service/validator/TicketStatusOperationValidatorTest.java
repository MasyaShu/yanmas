package ru.itterminal.yanmas.tickets.service.validator;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.yanmas.aau.model.Account;
import ru.itterminal.yanmas.commons.exception.LogicalValidationException;
import ru.itterminal.yanmas.commons.exception.error.ValidationError;
import ru.itterminal.yanmas.security.config.TestSecurityConfig;
import ru.itterminal.yanmas.tickets.model.TicketStatus;
import ru.itterminal.yanmas.tickets.model.projection.TicketStatusUniqueFields;
import ru.itterminal.yanmas.tickets.repository.TicketStatusRepository;

import java.util.*;

import static java.util.Collections.singletonList;
import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static ru.itterminal.yanmas.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;

@SpringJUnitConfig(value = {TicketStatusOperationValidator.class})
@Import(TestSecurityConfig.class)
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class TicketStatusOperationValidatorTest {
    @MockBean
    private TicketStatusRepository repository;

    @MockBean
    private TicketStatusUniqueFields ticketStatusUniqueFields;

    @Autowired
    private final TicketStatusOperationValidator validator = new TicketStatusOperationValidator(repository);

    private static final String EXIST_NAME = "ticketStatuss1";
    private static final String VALIDATED_FIELDS = "name";
    private static final Map<String, List<ValidationError>> errors = new HashMap<>();
    private static TicketStatus ticketStatus;

    @BeforeAll
    static void setUp() {
        Account account1 = new Account();
        account1.setId(UUID.fromString("bcf98101-2a22-42bf-94cc-c900b50a0b69"));
        ticketStatus = TicketStatus
                .builder()
                .name(EXIST_NAME)
                .account(account1)
                .build();
    }


    @Test
    void checkUniqueness_shouldGetTrue_whenPassedDataIsUnique() {
        when(repository.getByNameAndAccount_IdAndIdNot(any(), any(), any())).thenReturn(Collections.emptyList());
        assertTrue(validator.checkUniqueness(TicketStatus.builder().account(new Account()).build()));
    }

    @Test
    void checkUniqueness_shouldGetLogicalValidationException_whenPassedDataNotUnique() {
        when(repository.getByNameAndAccount_IdAndIdNot(any(), any(), any())).thenReturn(List.of(ticketStatusUniqueFields));
        when(ticketStatusUniqueFields.getName()).thenReturn(EXIST_NAME);
        errors.put(VALIDATED_FIELDS, singletonList(new ValidationError(VALIDATED_FIELDS, "name is occupied")));
        LogicalValidationException logicalValidationException = new LogicalValidationException(VALIDATED_FIELDS, errors);
        LogicalValidationException thrown = assertThrows(LogicalValidationException.class,
                () -> validator.checkUniqueness(ticketStatus));
        assertEquals(logicalValidationException.getFieldErrors().get("name").get(0),
                thrown.getFieldErrors().get("name").get(0));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void beforeCreate_shouldGetAccessDeniedException_whenCurrentUserFromNotInnerGroup() {
        AccessDeniedException thrown = assertThrows(AccessDeniedException.class,
                () -> validator.checkAccessBeforeCreate(ticketStatus));
        Assertions.assertEquals(TicketStatusOperationValidator.A_USER_FROM_NOT_INNER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_STATUS,
                thrown.getMessage());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void beforeUpdate_shouldGetAccessDeniedException_whenCurrentUserFromNotInnerGroup() {
        AccessDeniedException thrown = assertThrows(AccessDeniedException.class,
                () -> validator.checkAccessBeforeUpdate(ticketStatus));
        Assertions.assertEquals(TicketStatusOperationValidator.A_USER_FROM_NOT_INNER_GROUP_CANNOT_CREATE_OR_UPDATE_TICKET_STATUS,
                thrown.getMessage());
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeCreate_shouldGetTrue_whenCurrentUserFromInnerGroup() {
        assertTrue(validator.logicalValidationBeforeCreate(ticketStatus));
    }

    @Test
    @WithUserDetails("ADMIN_ACCOUNT_1_IS_INNER_GROUP")
    void beforeUpdate_shouldGetTrue_whenCurrentUserFromInnerGroup() {
        assertTrue(validator.logicalValidationBeforeUpdate(ticketStatus));
    }


}
