package ru.itterminal.botdesk.integration.aws.ses;

import static ru.itterminal.botdesk.integration.util.IntegrationConstants.REGION;

import java.util.Properties;

import javax.mail.Session;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.ses.SesClient;

@Configuration
public class AwsSesConfig {

    @Bean
    public SesClient getAmazonSesClient() {
        return SesClient.builder()
                .region(Region.of(REGION))
                .build();
    }

    @Bean
    public Session getMailSession() {
        return Session.getDefaultInstance(new Properties());
    }

}
