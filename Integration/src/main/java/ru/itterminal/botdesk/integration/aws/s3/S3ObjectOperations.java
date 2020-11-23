package ru.itterminal.botdesk.integration.aws.s3;

import java.nio.ByteBuffer;
import java.util.ArrayList;

import org.springframework.stereotype.Component;

import lombok.extern.slf4j.Slf4j;
import software.amazon.awssdk.core.sync.RequestBody;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.model.Delete;
import software.amazon.awssdk.services.s3.model.DeleteObjectsRequest;
import software.amazon.awssdk.services.s3.model.ObjectIdentifier;
import software.amazon.awssdk.services.s3.model.PutObjectRequest;
import software.amazon.awssdk.services.s3.model.S3Exception;

@Component
@Slf4j
public class S3ObjectOperations {

    private final S3Client s3Client;

    public S3ObjectOperations(S3Client s3Client) {
        this.s3Client = s3Client;
    }

    public boolean saveObject(String bucketName, String objectName, ByteBuffer byteBuffer) {
        PutObjectRequest objectRequest = PutObjectRequest.builder()
                .bucket(bucketName)
                .key(objectName)
                .build();
        try {
            s3Client.putObject(objectRequest, RequestBody.fromByteBuffer(byteBuffer));
        } catch (S3Exception e) {
            log.error(e.awsErrorDetails().errorMessage());
            return false;
        }
        return true;
    }

    public boolean deleteObject(String bucketName, String objectName) {
        ArrayList<ObjectIdentifier> toDelete = new ArrayList<>();
        toDelete.add(ObjectIdentifier.builder().key(objectName).build());
        try {
            DeleteObjectsRequest deleteObjectsRequest = DeleteObjectsRequest.builder()
                    .bucket(bucketName)
                    .delete(Delete.builder().objects(toDelete).build())
                    .build();
            s3Client.deleteObjects(deleteObjectsRequest);
        } catch (S3Exception e) {
            log.error(e.awsErrorDetails().errorMessage());
            return false;
        }
        return true;
    }

}
