package ru.itterminal.botdesk.integration.aws.s3;

import static ru.itterminal.botdesk.integration.aws.AwsConfig.REGION;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.s3.S3Client;

@Configuration
public class AwsS3Config {

    @Bean
    public S3Client getAmazonS3Client() {
        return S3Client.builder()
                .region(Region.of(REGION))
                .build();
    }
}
