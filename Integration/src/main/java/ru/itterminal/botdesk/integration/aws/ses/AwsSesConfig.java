package ru.itterminal.botdesk.integration.aws.ses;

import java.util.Properties;

import javax.mail.Session;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import lombok.RequiredArgsConstructor;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.ses.SesClient;

@Configuration
@RequiredArgsConstructor
public class AwsSesConfig {

    private final Region region;

    @Bean
    public SesClient getAmazonSesClient() {
        return SesClient.builder()
                .region(region)
                .build();
    }

    @Bean
    public Session getMailSession() {
        return Session.getDefaultInstance(new Properties());
    }

}
