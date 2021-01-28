package ru.itterminal.botdesk.tickets.service.impl;

import org.awaitility.Durations;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.integration.config.IntegrationConfig;
import ru.itterminal.botdesk.integration.innerflow.CompletedVerificationAccountFlow;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.model.test.TicketTypeTestHelper;
import ru.itterminal.botdesk.tickets.repository.TicketTypeRepository;
import ru.itterminal.botdesk.tickets.service.validator.TicketTypeOperationValidator;

import java.util.Optional;
import java.util.UUID;

import static org.awaitility.Awaitility.await;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@SpringJUnitConfig(value = {IntegrationConfig.class, CompletedVerificationAccountFlow.class, TicketTypeServiceImpl.class, TicketStatusServiceImpl.class})
class TicketTypeServiceImplCompletedVerificationFlowTest {

    @SuppressWarnings("SpringJavaInjectionPointsAutowiringInspection")
    @Autowired
    private CompletedVerificationAccountFlow.CreateCompletedVerificationAccountGateway createCompletedVerificationAccountGateway;

    @MockBean
    TicketTypeServiceImpl ticketTypeService;

    @MockBean
    TicketStatusServiceImpl ticketStatusService;

    @Test
    void createAwsBucketFlow_shouldCreateAwsBucket_whenPassedUniqueBucketName() {
        UUID uuid = UUID.randomUUID();
        createCompletedVerificationAccountGateway.process(uuid);
        await().pollDelay(Durations.ONE_SECOND).until(() -> true);
        verify(ticketTypeService, times(1)).createPredefinedEntity(uuid);
        verify(ticketStatusService, times(1)).createPredefinedEntity(uuid);
    }
}