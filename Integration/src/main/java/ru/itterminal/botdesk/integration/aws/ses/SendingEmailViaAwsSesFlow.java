package ru.itterminal.botdesk.integration.aws.ses;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.integration.annotation.Gateway;
import org.springframework.integration.annotation.MessagingGateway;
import org.springframework.integration.annotation.Poller;
import org.springframework.integration.annotation.ServiceActivator;
import org.springframework.integration.channel.QueueChannel;
import org.springframework.messaging.MessageChannel;

import software.amazon.awssdk.services.ses.model.SendRawEmailRequest;

@Configuration
public class SendingEmailViaAwsSesFlow {

    private final SenderEmailViaAwsSes senderEmailViaAwsSes;

    public SendingEmailViaAwsSesFlow(SenderEmailViaAwsSes senderEmailViaAwsSes) {
        this.senderEmailViaAwsSes = senderEmailViaAwsSes;
    }

    @Bean
    public MessageChannel mailSenderViaAwsSesChannel() {
        return new QueueChannel(1000);
    }

    @MessagingGateway
    public interface MailSenderViaAwsSesMessagingGateway {
        @Gateway(requestChannel = "mailSenderViaAwsSesChannel")
        String process(SendRawEmailRequest email);
    }

    @ServiceActivator(inputChannel = "mailSenderViaAwsSesChannel",
            poller = @Poller(maxMessagesPerPoll = "14", fixedRate = "1000"))
    private String sendEmail(SendRawEmailRequest rawEmailRequest) {
        return senderEmailViaAwsSes.sendEmail(rawEmailRequest);
    }

}
