package ru.itterminal.botdesk.integration.aws.s3;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.ByteBuffer;
import java.util.Random;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import ru.itterminal.botdesk.integration.aws.AwsConfig;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.model.Bucket;
import software.amazon.awssdk.services.s3.model.ListBucketsResponse;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@SpringBootTest(classes = {AwsConfig.class, AwsS3Config.class, AwsS3BucketOperations.class, AwsS3ObjectOperations.class})
class S3BucketAndObjectOperationsTest {

    @Autowired
    AwsS3BucketOperations awsS3BucketOperations;

    @Autowired
    AwsS3ObjectOperations awsS3ObjectOperations;

    @Autowired
    S3Client s3Client;

    private final String nameBucket = "cdfa6483-0769-4628-ba32-efd338a716de";
    private byte[] data;

    @BeforeAll
    void setUp() {
        data = new byte[10];
        new Random().nextBytes(data);
    }

    @Test
    @Order(10)
    void createBucket_shouldCreateBucket_whenPassedNameBucketIsUnique() {
        assertTrue(awsS3BucketOperations.createBucket(nameBucket));
    }

    @Test
    @Order(20)
    void putObject_shouldPutObject_whenPassedValidData() {
        assertTrue(awsS3ObjectOperations.putObject(nameBucket, "test.txt", ByteBuffer.wrap(data)));
    }

    @Test
    @Order(25)
    void putObject_shouldPutObject_whenSizeOfFileIsZero() {
        assertTrue(awsS3ObjectOperations.putObject(nameBucket, "empty_test.txt", ByteBuffer.wrap(new byte[0])));
    }

    @Test
    @Order(30)
    void getObject_shouldGetObject_whenPassedValidParameters() {
        byte[] dataFromS3 = awsS3ObjectOperations.getObject(nameBucket, "test.txt");
        assertArrayEquals(data, dataFromS3);
    }

    @Test
    @Order(40)
    void deleteObject_shouldDeleteObject_whenPassedValidData1() {
        assertTrue(awsS3ObjectOperations.deleteObject(nameBucket, "test.txt"));
    }

    @Test
    @Order(45)
    void deleteObject_shouldDeleteObject_whenPassedValidData2() {
        assertTrue(awsS3ObjectOperations.deleteObject(nameBucket, "empty_test.txt"));
    }

    @Test
    @Order(50)
    void deleteBucket_shouldDeleteBucket_whenBucketWithPassedNameExist() {
        assertTrue(awsS3BucketOperations.deleteBucket(nameBucket));
    }

    @SuppressWarnings("unused")
    void deleteAllBuckets () {
        ListBucketsResponse buckets = s3Client.listBuckets();
        System.out.println("There are buckets are, they will delete now:");
        for (Bucket b : buckets.buckets()) {
            System.out.println("* " + b.name());
            awsS3BucketOperations.deleteBucket(b.name());
        }
    }
}