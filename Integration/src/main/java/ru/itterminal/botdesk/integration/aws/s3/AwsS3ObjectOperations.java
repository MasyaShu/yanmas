package ru.itterminal.botdesk.integration.aws.s3;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import software.amazon.awssdk.core.ResponseBytes;
import software.amazon.awssdk.core.sync.RequestBody;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.model.Delete;
import software.amazon.awssdk.services.s3.model.DeleteObjectsRequest;
import software.amazon.awssdk.services.s3.model.GetObjectRequest;
import software.amazon.awssdk.services.s3.model.GetObjectResponse;
import software.amazon.awssdk.services.s3.model.ObjectIdentifier;
import software.amazon.awssdk.services.s3.model.PutObjectRequest;
import software.amazon.awssdk.services.s3.model.S3Exception;
import software.amazon.awssdk.services.s3.model.StorageClass;

@Component
@Slf4j
@RequiredArgsConstructor
public class AwsS3ObjectOperations {

    private final S3Client s3Client;

    @Value("${aws.s3.bucket.name}")
    private String bucketName;

    public boolean putObject(UUID accountId, UUID fileId, ByteBuffer byteBuffer) {
        var objectRequest = PutObjectRequest.builder()
                .bucket(bucketName)
                .storageClass(StorageClass.STANDARD_IA)
                .key(accountId.toString() + "/" + fileId.toString())
                .build();
        try {
            s3Client.putObject(objectRequest, RequestBody.fromByteBuffer(byteBuffer));
        }
        catch (S3Exception e) {
            log.error(e.awsErrorDetails().errorMessage());
            return false;
        }
        return true;
    }

    public byte[] getObject(UUID accountId, UUID fileId) {
        byte[] data = {};
        try {
            GetObjectRequest objectRequest = GetObjectRequest
                    .builder()
                    .key(accountId.toString() + "/" + fileId.toString())
                    .bucket(bucketName)
                    .build();
            ResponseBytes<GetObjectResponse> objectBytes = s3Client.getObjectAsBytes(objectRequest);
            data = objectBytes.asByteArray();
            return data;
        }
        catch (S3Exception e) {
            log.error(e.awsErrorDetails().errorMessage());
            return data;
        }
    }

    public boolean deleteObject(UUID accountId, UUID fileId) {
        ArrayList<ObjectIdentifier> toDelete = new ArrayList<>();
        toDelete.add(ObjectIdentifier.builder().key(accountId.toString() + "/" + fileId.toString()).build());
        try {
            DeleteObjectsRequest deleteObjectsRequest = DeleteObjectsRequest.builder()
                    .bucket(bucketName)
                    .delete(Delete.builder().objects(toDelete).build())
                    .build();
            s3Client.deleteObjects(deleteObjectsRequest);
        }
        catch (S3Exception e) {
            log.error(e.awsErrorDetails().errorMessage());
            return false;
        }
        return true;
    }
}
