package ru.itterminal.botdesk.integration.aws;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import software.amazon.awssdk.regions.Region;

@Configuration
public class AwsConfig {

    public static final String REGION = "eu-central-1";

    @Bean
    public Region getAwsRegion() {
        return Region.of(REGION);
    }
}
