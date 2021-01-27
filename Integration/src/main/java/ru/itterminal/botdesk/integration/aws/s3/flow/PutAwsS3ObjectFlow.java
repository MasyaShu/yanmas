package ru.itterminal.botdesk.integration.aws.s3.flow;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.integration.annotation.Gateway;
import org.springframework.integration.annotation.MessagingGateway;
import org.springframework.integration.annotation.Poller;
import org.springframework.integration.annotation.ServiceActivator;
import org.springframework.integration.channel.QueueChannel;
import org.springframework.messaging.MessageChannel;

import lombok.RequiredArgsConstructor;
import ru.itterminal.botdesk.integration.aws.s3.AwsS3Object;
import ru.itterminal.botdesk.integration.aws.s3.AwsS3ObjectOperations;

@Configuration
@RequiredArgsConstructor
public class PutAwsS3ObjectFlow {

    private final AwsS3ObjectOperations awsS3ObjectOperations;

    @Bean
    public MessageChannel putAwsS3ObjectChannel() {
        return new QueueChannel(1000);
    }

    @MessagingGateway
    public interface PutAwsS3ObjectGateway {
        @Gateway(requestChannel = "putAwsS3ObjectChannel")
        void process(AwsS3Object awsS3Object);
    }

    @ServiceActivator(inputChannel = "putAwsS3ObjectChannel",
            poller = @Poller(maxMessagesPerPoll = "1", fixedRate = "100"))
    private void putAwsS3Object(AwsS3Object awsS3Object) {
        awsS3ObjectOperations.putObject(awsS3Object.getBucketName(), awsS3Object.getObjectName(), awsS3Object.getByteBuffer());
    }

}
