package ru.itterminal.botdesk.integration.aws.s3;

import org.springframework.stereotype.Component;

import lombok.extern.slf4j.Slf4j;
import software.amazon.awssdk.core.waiters.WaiterResponse;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.model.CreateBucketConfiguration;
import software.amazon.awssdk.services.s3.model.CreateBucketRequest;
import software.amazon.awssdk.services.s3.model.DeleteBucketRequest;
import software.amazon.awssdk.services.s3.model.HeadBucketRequest;
import software.amazon.awssdk.services.s3.model.HeadBucketResponse;
import software.amazon.awssdk.services.s3.model.S3Exception;
import software.amazon.awssdk.services.s3.waiters.S3Waiter;

@Component
@Slf4j
public class S3BucketOperations {

    private final S3Client s3Client;
    private final Region region;

    public S3BucketOperations(S3Client s3Client, Region region) {
        this.s3Client = s3Client;
        this.region = region;
    }

    public boolean createBucket(String bucketName) {
        try {
            S3Waiter s3Waiter = s3Client.waiter();
            CreateBucketRequest bucketRequest = CreateBucketRequest.builder()
                    .bucket(bucketName)
                    .createBucketConfiguration(
                            CreateBucketConfiguration.builder()
                                    .locationConstraint(region.id())
                                    .build())
                    .build();

            s3Client.createBucket(bucketRequest);
            HeadBucketRequest bucketRequestWait = HeadBucketRequest.builder()
                    .bucket(bucketName)
                    .build();

            // Wait until the bucket is created and logging the response
            WaiterResponse<HeadBucketResponse> waiterResponse = s3Waiter.waitUntilBucketExists(bucketRequestWait);
            waiterResponse.matched().response().ifPresent(response -> log.trace(String.valueOf(response)));
            log.trace("bucket: " + bucketName + " is ready");
        }
        catch (S3Exception e) {
            log.error(e.awsErrorDetails().errorMessage());
            return false;
        }
        return true;
    }

    public boolean deleteBucket(String bucketName) {
        DeleteBucketRequest deleteBucketRequest = DeleteBucketRequest.builder().bucket(bucketName).build();
        try {
            s3Client.deleteBucket(deleteBucketRequest);
        }
        catch (S3Exception e) {
            log.error(e.awsErrorDetails().errorMessage());
            return false;
        }
        return true;
    }

}
