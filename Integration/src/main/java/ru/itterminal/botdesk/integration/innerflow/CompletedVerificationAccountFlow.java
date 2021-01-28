package ru.itterminal.botdesk.integration.innerflow;

import lombok.RequiredArgsConstructor;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.integration.annotation.Gateway;
import org.springframework.integration.annotation.MessagingGateway;
import org.springframework.integration.annotation.Poller;
import org.springframework.integration.annotation.ServiceActivator;
import org.springframework.integration.channel.QueueChannel;
import org.springframework.messaging.MessageChannel;

import java.util.UUID;

@Configuration
@RequiredArgsConstructor
public class CompletedVerificationAccountFlow {

    private final ApplicationContext appContext;

    @Bean
    public MessageChannel createCompletedVerificationAccountChannel() {
        return new QueueChannel(100);
    }

    @MessagingGateway
    public interface CreateCompletedVerificationAccountGateway {
        @Gateway(requestChannel = "createCompletedVerificationAccountChannel")
        void process(UUID accountId);
    }

    @ServiceActivator(inputChannel = "createCompletedVerificationAccountChannel",
            poller = @Poller(maxMessagesPerPoll = "1", fixedRate = "1000"))
    private void createPredefinedEntity(UUID uuid) {
        CompletedVerificationAccount ticketTypeServiceImpl =
                (CompletedVerificationAccount) appContext.getBean("ticketTypeServiceImpl");
        ticketTypeServiceImpl.createPredefinedEntity(uuid);

        CompletedVerificationAccount ticketStatusServiceImpl =
                (CompletedVerificationAccount) appContext.getBean("ticketStatusServiceImpl");
        ticketStatusServiceImpl.createPredefinedEntity(uuid);

    }
}
