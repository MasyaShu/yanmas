package ru.itterminal.botdesk.integration.aws.s3;

import java.nio.ByteBuffer;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class AwsS3Object {
    private String bucketName;
    private String objectName;
    private ByteBuffer byteBuffer;
}
