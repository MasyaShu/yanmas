package ru.itterminal.botdesk.integration.aws.s3.flow;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.integration.annotation.Gateway;
import org.springframework.integration.annotation.MessagingGateway;
import org.springframework.integration.annotation.Poller;
import org.springframework.integration.annotation.ServiceActivator;
import org.springframework.integration.channel.QueueChannel;
import org.springframework.messaging.MessageChannel;

import ru.itterminal.botdesk.integration.aws.s3.AwsS3Object;
import ru.itterminal.botdesk.integration.aws.s3.AwsS3ObjectOperations;

@Configuration
public class DeleteAwsS3ObjectFlow {

    private final AwsS3ObjectOperations awsS3ObjectOperations;

    public DeleteAwsS3ObjectFlow(AwsS3ObjectOperations awsS3ObjectOperations) {
        this.awsS3ObjectOperations = awsS3ObjectOperations;
    }

    @Bean
    public MessageChannel deleteAwsS3ObjectChannel() {
        return new QueueChannel(100);
    }

    @MessagingGateway
    public interface DeleteAwsS3ObjectGateway {
        @Gateway(requestChannel = "deleteAwsS3ObjectChannel")
        void process(AwsS3Object awsS3Object);
    }

    @ServiceActivator(inputChannel = "deleteAwsS3ObjectChannel",
            poller = @Poller(maxMessagesPerPoll = "1", fixedRate = "100"))
    private void deleteAwsS3Object(AwsS3Object awsS3Object) {
        awsS3ObjectOperations.deleteObject(awsS3Object.getBucketName(), awsS3Object.getObjectName());
    }

}
