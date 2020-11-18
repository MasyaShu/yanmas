package ru.itterminal.botdesk.integration.aws;

import java.time.Duration;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.integration.annotation.Gateway;
import org.springframework.integration.annotation.MessagingGateway;
import org.springframework.integration.annotation.ServiceActivator;
import org.springframework.integration.channel.QueueChannel;
import org.springframework.integration.handler.advice.RateLimiterRequestHandlerAdvice;
import org.springframework.messaging.MessageChannel;

import com.amazonaws.regions.Regions;
import com.amazonaws.services.simpleemail.AmazonSimpleEmailService;
import com.amazonaws.services.simpleemail.AmazonSimpleEmailServiceClientBuilder;
import com.amazonaws.services.simpleemail.model.SendEmailRequest;

import io.github.resilience4j.ratelimiter.RateLimiterConfig;

@Configuration
public class SenderEmailViaAwsSes {

    @Value("${aws.ses.region}")
    private String region;

    @Value("${aws.ses.email.noReplay}")
    private String emailNoReplay;

    private static AmazonSimpleEmailService amazonSimpleEmailService = null;

    @Bean
    public MessageChannel mailSenderViaAwsSesChannel() {
        return new QueueChannel(1000);
    }

    @MessagingGateway
    public interface MailSenderViaAwsSesMessagingGateway {
        @Gateway(requestChannel = "mailSenderViaAwsSesChannel")
        String process(SendEmailRequest email);
    }

    @Bean
    public RateLimiterRequestHandlerAdvice rateLimiterSendEmail() {
        return new RateLimiterRequestHandlerAdvice(
                RateLimiterConfig.custom()
                        .limitRefreshPeriod(Duration.ofSeconds(1))
                        .limitForPeriod(14)
                        .build());
    }

    @ServiceActivator(inputChannel = "mailSenderViaAwsSesChannel", adviceChain = "rateLimiterSendEmail")
    public String sendEmail(SendEmailRequest email) {
        if (email.getSource() == null) {
            email.setSource(emailNoReplay);
        }
        return getAmazonSimpleEmailService(region).sendEmail(email).getMessageId();
    }

    private static AmazonSimpleEmailService getAmazonSimpleEmailService(String region) {
        if (amazonSimpleEmailService == null) {
            amazonSimpleEmailService = AmazonSimpleEmailServiceClientBuilder
                    .standard()
                    .withRegion(Regions.fromName(region))
                    .build();
        }
        return amazonSimpleEmailService;
    }
}
