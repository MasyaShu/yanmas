package ru.itterminal.botdesk.integration.aws.s3.flow;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.integration.annotation.Gateway;
import org.springframework.integration.annotation.MessagingGateway;
import org.springframework.integration.annotation.Poller;
import org.springframework.integration.annotation.ServiceActivator;
import org.springframework.integration.channel.QueueChannel;
import org.springframework.messaging.MessageChannel;

import ru.itterminal.botdesk.integration.aws.s3.S3BucketOperations;

@Configuration
public class DeleteAwsS3BucketFlow {

    private final S3BucketOperations s3BucketOperations;

    public DeleteAwsS3BucketFlow(S3BucketOperations s3BucketOperations) {
        this.s3BucketOperations = s3BucketOperations;
    }

    @Bean
    public MessageChannel deleteAwsBucketChannel() {
        return new QueueChannel(100);
    }

    @MessagingGateway
    public interface DeleteAwsBucketGateway {
        @Gateway(requestChannel = "deleteAwsBucketChannel")
        void process(String bucketName);
    }

    @ServiceActivator(inputChannel = "deleteAwsBucketChannel",
            poller = @Poller(maxMessagesPerPoll = "1", fixedRate = "1000"))
    private void deleteAwsBucket(String bucketName) {
        s3BucketOperations.deleteBucket(bucketName);
    }

}
