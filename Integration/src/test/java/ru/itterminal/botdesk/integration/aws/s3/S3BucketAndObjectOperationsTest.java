package ru.itterminal.botdesk.integration.aws.s3;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.nio.ByteBuffer;
import java.util.Random;
import java.util.UUID;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Order;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestMethodOrder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

import ru.itterminal.botdesk.integration.aws.AwsConfig;
import software.amazon.awssdk.services.s3.S3Client;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
@SpringBootTest(classes = {AwsConfig.class, AwsS3Config.class, AwsS3ObjectOperations.class})
@TestPropertySource(properties = {"aws.s3.bucket.name=unit-tests.botdesk.app"})
class S3BucketAndObjectOperationsTest {

    @Autowired
    AwsS3ObjectOperations awsS3ObjectOperations;

    @Autowired
    S3Client s3Client;

    private byte[] data;
    private final UUID accountId = UUID.fromString("29ef72ac-b94d-4c98-b6f6-efa765f44b2e");
    private final UUID fileId = UUID.fromString("a4e76f8b-97e0-4aa6-89ba-1415f1f05af0");
    private final UUID emptyFileId = UUID.fromString("d3e2fe4d-b031-48ad-a0ea-d1650bbbd8c8");


    @BeforeAll
    void setUp() {
        data = new byte[10];
        new Random().nextBytes(data);
    }

    @Test
    @Order(10)
    void putObject_shouldPutObject_whenPassedValidData() {
        assertTrue(awsS3ObjectOperations.putObject(accountId, fileId, ByteBuffer.wrap(data)));
    }

    @Test
    @Order(20)
    void putObject_shouldPutObject_whenSizeOfFileIsZero() {
        assertTrue(awsS3ObjectOperations.putObject(accountId, emptyFileId, ByteBuffer.wrap(new byte[0])));
    }

    @Test
    @Order(30)
    void getObject_shouldGetObject_whenPassedValidParameters() {
        byte[] dataFromS3 = awsS3ObjectOperations.getObject( accountId, fileId);
        assertArrayEquals(data, dataFromS3);
    }

    @Test
    @Order(40)
    void deleteObject_shouldDeleteObject_whenPassedValidData1() {
        assertTrue(awsS3ObjectOperations.deleteObject( accountId, fileId));
    }

    @Test
    @Order(50)
    void deleteObject_shouldDeleteObject_whenPassedValidData2() {
        assertTrue(awsS3ObjectOperations.deleteObject( accountId, emptyFileId));
    }

}