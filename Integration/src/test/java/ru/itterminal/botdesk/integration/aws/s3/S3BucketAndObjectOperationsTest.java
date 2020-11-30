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

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@SpringBootTest(classes = {AwsConfig.class, AwsS3Config.class, S3BucketOperations.class, S3ObjectOperations.class})
class S3BucketAndObjectOperationsTest {

    @Autowired
    S3BucketOperations s3BucketOperations;

    @Autowired
    S3ObjectOperations s3ObjectOperations;

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
        assertTrue(s3BucketOperations.createBucket(nameBucket));
    }

    @Test
    @Order(20)
    void putObject_shouldPutObject_whenPassedValidData() {
        assertTrue(s3ObjectOperations.putObject(nameBucket, "test.txt", ByteBuffer.wrap(data)));
    }

    @Test
    @Order(30)
    void getObject_shouldGetObject_whenPassedValidParameters() {
        byte[] dataFromS3 = s3ObjectOperations.getObject(nameBucket, "test.txt");
        assertArrayEquals(data, dataFromS3);
    }

    @Test
    @Order(40)
    void deleteObject_shouldDeleteObject_whenPassedValidData() {
        assertTrue(s3ObjectOperations.deleteObject(nameBucket, "test.txt"));
    }

    @Test
    @Order(50)
    void deleteBucket_shouldDeleteBucket_whenBucketWithPassedNameExist() {
        assertTrue(s3BucketOperations.deleteBucket(nameBucket));
    }
}