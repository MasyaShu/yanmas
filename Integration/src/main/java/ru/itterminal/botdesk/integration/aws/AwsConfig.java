package ru.itterminal.botdesk.integration.aws;

import static ru.itterminal.botdesk.integration.util.IntegrationConstants.REGION;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import software.amazon.awssdk.regions.Region;

@Configuration
public class AwsConfig {

    @Bean
    public Region getAwsRegion() {
        return Region.of(REGION);
    }
}
