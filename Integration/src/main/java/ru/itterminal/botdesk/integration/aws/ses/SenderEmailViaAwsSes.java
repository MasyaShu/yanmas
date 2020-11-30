package ru.itterminal.botdesk.integration.aws.ses;

import static ru.itterminal.botdesk.integration.util.IntegrationConstants.NO_REPLY_BOTDESK_APP;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;

import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeBodyPart;
import javax.mail.internet.MimeMessage;
import javax.mail.internet.MimeMultipart;

import org.springframework.stereotype.Component;

import software.amazon.awssdk.core.SdkBytes;
import software.amazon.awssdk.services.ses.SesClient;
import software.amazon.awssdk.services.ses.model.RawMessage;
import software.amazon.awssdk.services.ses.model.SendRawEmailRequest;

@Component
public class SenderEmailViaAwsSes {

    private final SesClient sesClient;
    private final Session mailSession;

    public SenderEmailViaAwsSes(SesClient sesClient, Session mailSession) {
        this.sesClient = sesClient;
        this.mailSession = mailSession;
    }

    public String sendEmail(SendRawEmailRequest rawEmailRequest) {
        return sesClient.sendRawEmail(rawEmailRequest).messageId();
    }

    public SendRawEmailRequest createEmail(String sender,
                                           String recipient,
                                           String subject,
                                           String bodyText,
                                           String bodyHTML) throws MessagingException, IOException {

        // Create a new MimeMessage object.
        MimeMessage message = new MimeMessage(mailSession);

        // Add subject, from and to lines.
        message.setSubject(subject, "UTF-8");
        if (sender == null || sender.isEmpty()) {
            sender = NO_REPLY_BOTDESK_APP;
        }
        message.setFrom(new InternetAddress(sender));
        message.setRecipients(Message.RecipientType.TO, InternetAddress.parse(recipient));

        // Create a multipart/alternative child container.
        MimeMultipart msgBody = new MimeMultipart("alternative");

        // Create a wrapper for the HTML and text parts.
        MimeBodyPart wrap = new MimeBodyPart();

        // Define the text part.
        MimeBodyPart textPart = new MimeBodyPart();
        textPart.setContent(bodyText, "text/plain; charset=UTF-8");

        // Define the HTML part.
        MimeBodyPart htmlPart = new MimeBodyPart();
        htmlPart.setContent(bodyHTML, "text/html; charset=UTF-8");

        // Add the text and HTML parts to the child container.
        msgBody.addBodyPart(textPart);
        msgBody.addBodyPart(htmlPart);

        // Add the child container to the wrapper object.
        wrap.setContent(msgBody);

        // Create a multipart/mixed parent container.
        MimeMultipart msg = new MimeMultipart("mixed");

        // Add the parent container to the message.
        message.setContent(msg);

        // Add the multipart/alternative part to the message.
        msg.addBodyPart(wrap);

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        message.writeTo(outputStream);

        ByteBuffer buf = ByteBuffer.wrap(outputStream.toByteArray());

        byte[] arr = new byte[buf.remaining()];
        buf.get(arr);

        SdkBytes data = SdkBytes.fromByteArray(arr);

        RawMessage rawMessage =  RawMessage.builder()
                .data(data)
                .build();

        return SendRawEmailRequest.builder()
                .rawMessage(rawMessage)
                .build();
    }
}
