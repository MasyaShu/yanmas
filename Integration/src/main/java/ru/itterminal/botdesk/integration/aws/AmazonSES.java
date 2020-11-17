package ru.itterminal.botdesk.integration.aws;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.amazonaws.regions.Regions;
import com.amazonaws.services.simpleemail.AmazonSimpleEmailService;
import com.amazonaws.services.simpleemail.AmazonSimpleEmailServiceClientBuilder;
import com.amazonaws.services.simpleemail.model.Destination;
import com.amazonaws.services.simpleemail.model.Message;
import com.amazonaws.services.simpleemail.model.SendEmailRequest;


@Service
public class AmazonSES {

    @Value("${aws.ses.email.noReplay}")
    private String emailNoReplay;

    @Value("${aws.ses.region}")
    private String region;

    public String sendEmail(Message message, String toAddress) {
        AmazonSimpleEmailService client = AmazonSimpleEmailServiceClientBuilder.standard()
                .withRegion(Regions.fromName(region))
                .build();
        SendEmailRequest request = new SendEmailRequest()
                .withDestination(new Destination().withToAddresses(toAddress))
                .withMessage(message)
                .withSource(emailNoReplay);
        return client.sendEmail(request).getMessageId();
    }

}
