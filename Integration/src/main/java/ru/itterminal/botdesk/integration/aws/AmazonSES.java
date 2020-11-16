package ru.itterminal.botdesk.integration.aws;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.amazonaws.regions.Regions;
import com.amazonaws.services.simpleemail.AmazonSimpleEmailService;
import com.amazonaws.services.simpleemail.AmazonSimpleEmailServiceClientBuilder;
import com.amazonaws.services.simpleemail.model.Body;
import com.amazonaws.services.simpleemail.model.Content;
import com.amazonaws.services.simpleemail.model.Destination;
import com.amazonaws.services.simpleemail.model.Message;
import com.amazonaws.services.simpleemail.model.SendEmailRequest;


@Service
public class AmazonSES {

    @Value("${aws.ses.email.noReplay}")
    private String emailNoReplay;

    @Value("${aws.ses.region}")
    private String region;

    @Value("${aws.accessKeyId}")
    private String accessKeyId;

    @Value("${aws.secretKey}")
    private String secretKey;

    public AmazonSES() {
    }

    @PostConstruct
    void setSystemProperty(){
        System.setProperty("aws.accessKeyId", accessKeyId);
        System.setProperty("aws.secretKey", secretKey);
    }

    public void sendEmail(UserDto userDto) {

        String SUBJECT = messages.getMessage("amazon.ses.emailVerification.subject")
                + " " + projectName;

        String HTMLBODY = messages.getMessage("amazon.ses.emailVerification.HTMLBody");

        String TEXTBODY = messages.getMessage("amazon.ses.emailVerification.TextBody");

        AmazonSimpleEmailService client = AmazonSimpleEmailServiceClientBuilder.standard()
                .withRegion(Regions.fromName(region))
                .build();

        String htmlBodyWithToken = HTMLBODY.replace("$tokenValue", userDto.getEmailVerificationToken());
        htmlBodyWithToken = htmlBodyWithToken.replace("$AppUrl", appUrl);
        htmlBodyWithToken = htmlBodyWithToken.replace("$UrlEmailVerify", SecurityConstants.VERIFICATION_EMAIL_URL);

        String textBodyWithToken = TEXTBODY.replace("$tokenValue", userDto.getEmailVerificationToken());
        textBodyWithToken = textBodyWithToken.replace("$AppUrl", appUrl);
        textBodyWithToken = textBodyWithToken.replace("$UrlEmailVerify", SecurityConstants.VERIFICATION_EMAIL_URL);

        SendEmailRequest request = new SendEmailRequest()
                .withDestination(new Destination().withToAddresses(userDto.getEmail()))
                .withMessage(new Message()
                        .withBody(new Body().withHtml(new Content().withCharset("UTF-8").withData(htmlBodyWithToken))
                                .withText(new Content().withCharset("UTF-8").withData(textBodyWithToken)))
                        .withSubject(new Content().withCharset("UTF-8").withData(SUBJECT)))
                .withSource(emailNoReplay);

        client.sendEmail(request);
    }

}
