package ru.itterminal.botdesk.integration.aws;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.integration.annotation.Gateway;
import org.springframework.integration.annotation.MessagingGateway;
import org.springframework.integration.annotation.Poller;
import org.springframework.integration.annotation.ServiceActivator;
import org.springframework.integration.channel.QueueChannel;
import org.springframework.messaging.MessageChannel;

import com.amazonaws.regions.Regions;
import com.amazonaws.services.simpleemail.AmazonSimpleEmailService;
import com.amazonaws.services.simpleemail.AmazonSimpleEmailServiceClientBuilder;
import com.amazonaws.services.simpleemail.model.Body;
import com.amazonaws.services.simpleemail.model.Content;
import com.amazonaws.services.simpleemail.model.Destination;
import com.amazonaws.services.simpleemail.model.Message;
import com.amazonaws.services.simpleemail.model.SendEmailRequest;

@Configuration
public class SenderEmailViaAwsSes {

    @Value("${aws.ses.region}")
    private String region;

    @Value("${aws.ses.email.noReplay}")
    private String emailNoReplay;

    @Bean
    public AmazonSimpleEmailService getAmazonSimpleEmailService() {
        return AmazonSimpleEmailServiceClientBuilder
                .standard()
                .withRegion(Regions.fromName(region))
                .build();
    }

    private final AmazonSimpleEmailService amazonSimpleEmailService;

    public SenderEmailViaAwsSes(AmazonSimpleEmailService amazonSimpleEmailService) {
        this.amazonSimpleEmailService = amazonSimpleEmailService;
    }

    @MessagingGateway
    public interface MailSenderViaAwsSesMessagingGateway {
        @Gateway(requestChannel = "mailSenderViaAwsSesChannel")
        String process(SendEmailRequest email);
    }

    @Bean
    public MessageChannel mailSenderViaAwsSesChannel() {
        return new QueueChannel(1000);
    }

    @ServiceActivator(inputChannel = "mailSenderViaAwsSesChannel",
            poller = @Poller(maxMessagesPerPoll = "14", fixedRate = "1000"))
    private String sendEmail(SendEmailRequest email) {
        if (email.getSource() == null) {
            email.setSource(emailNoReplay);
        }
        return amazonSimpleEmailService.sendEmail(email).getMessageId();
    }


    public static SendEmailRequest createEmail(String toAddress, String subject, String textBody) {
        Message message = new Message()
                .withBody(new Body().withText(new Content().withCharset("UTF-8").withData(textBody)))
                .withSubject(new Content().withCharset("UTF-8").withData(subject));
        return  new SendEmailRequest()
                .withDestination(new Destination().withToAddresses(toAddress))
                .withMessage(message);
    }
}
