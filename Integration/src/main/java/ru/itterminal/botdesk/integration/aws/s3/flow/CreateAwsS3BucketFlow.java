package ru.itterminal.botdesk.integration.aws.s3.flow;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.integration.annotation.Gateway;
import org.springframework.integration.annotation.MessagingGateway;
import org.springframework.integration.annotation.Poller;
import org.springframework.integration.annotation.ServiceActivator;
import org.springframework.integration.channel.QueueChannel;
import org.springframework.messaging.MessageChannel;

import ru.itterminal.botdesk.integration.aws.s3.AwsS3BucketOperations;

@Configuration
public class CreateAwsS3BucketFlow {

    private final AwsS3BucketOperations awsS3BucketOperations;

    public CreateAwsS3BucketFlow(AwsS3BucketOperations awsS3BucketOperations) {
        this.awsS3BucketOperations = awsS3BucketOperations;
    }

    @Bean
    public MessageChannel createAwsBucketChannel() {
        return new QueueChannel(100);
    }

    @MessagingGateway
    public interface CreateAwsBucketGateway {
        @Gateway(requestChannel = "createAwsBucketChannel")
        void process(String bucketName);
    }

    @ServiceActivator(inputChannel = "createAwsBucketChannel",
            poller = @Poller(maxMessagesPerPoll = "1", fixedRate = "1000"))
    private void createAwsBucket(String bucketName) {
        awsS3BucketOperations.createBucket(bucketName);
    }

}
