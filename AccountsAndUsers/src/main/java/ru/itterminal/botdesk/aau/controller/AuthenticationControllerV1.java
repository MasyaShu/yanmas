package ru.itterminal.botdesk.aau.controller;

import static ru.itterminal.botdesk.aau.util.AAUConstants.EMAIL_PATTERN;
import static ru.itterminal.botdesk.aau.util.AAUConstants.INVALID_EMAIL;

import java.util.HashMap;
import java.util.Map;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Pattern;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import ru.itterminal.botdesk.aau.model.dto.AuthenticationRequestDto;
import ru.itterminal.botdesk.aau.model.dto.ResetPasswordDto;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.exception.JwtAuthenticationException;
import ru.itterminal.botdesk.security.jwt.JwtProvider;

@RestController
@RequestMapping(value = "/api/v1/auth")
@Validated
public class AuthenticationControllerV1 {

    public static final String TOKEN_FOR_UPDATE_EMAIL_WAS_SENT_TO_NEW_EMAIL =
            "token for update email was sent to new email";
    public static final String EMAIL_WAS_SUCCESSFULLY_UPDATED = "email was successfully updated";
    private final AuthenticationManager authenticationManager;

    private final JwtProvider jwtProvider;

    private final UserServiceImpl userService;

    public static final String INVALID_USERNAME_OR_PASSWORD = "invalid username or password";
    public static final String EMAIL_IS_VERIFIED = "email is verified";
    public static final String TOKEN_FOR_RESET_PASSWORD_WAS_SENT_TO_EMAIL =
            "token for reset password was sent to email";
    public static final String PASSWORD_WAS_RESET_SUCCESSFULLY = "password was reset successfully";

    @Autowired
    public AuthenticationControllerV1(AuthenticationManager authenticationManager, JwtProvider jwtProvider,
                                      UserServiceImpl userService) {
        this.authenticationManager = authenticationManager;
        this.jwtProvider = jwtProvider;
        this.userService = userService;
    }

    @PostMapping("/signin")
    public ResponseEntity<Map<Object, Object>> signIn(@RequestBody AuthenticationRequestDto requestDto) {
        try {
            String email = requestDto.getEmail();
            String password = requestDto.getPassword();
            authenticationManager.authenticate(new UsernamePasswordAuthenticationToken(email, password));
            Map<Object, Object> response = new HashMap<>();
            response.put("token", jwtProvider.createToken(email));
            return ResponseEntity.ok(response);
        }
        catch (Exception e) {
            throw new JwtAuthenticationException(INVALID_USERNAME_OR_PASSWORD);
        }
    }

    @GetMapping(path = "/email-verify")
    public ResponseEntity<String> verifyEmailToken(@RequestParam(value = "token") @NotEmpty String token) {
        userService.verifyEmailToken(token);
        return ResponseEntity.ok(EMAIL_IS_VERIFIED);
    }

    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER')")
    @GetMapping(path = "/request-email-update")
    public ResponseEntity<String> requestEmailUpdate(
            @Pattern(regexp = EMAIL_PATTERN, message = INVALID_EMAIL)
            @NotEmpty
            @RequestParam(value = "newEmail") String newEmail
    ) {
        userService.requestUpdateEmailOfAccountOwner(newEmail);
        return ResponseEntity.ok(TOKEN_FOR_UPDATE_EMAIL_WAS_SENT_TO_NEW_EMAIL);
    }

    @GetMapping(path = "/request-password-reset")
    public ResponseEntity<String> requestPasswordReset(@RequestParam(value = "email") @NotEmpty String email) {
        userService.requestPasswordReset(email);
        return ResponseEntity.ok(TOKEN_FOR_RESET_PASSWORD_WAS_SENT_TO_EMAIL);
    }

    @GetMapping(path = "/password-reset")
    public ResponseEntity<String> passwordReset(@Validated @RequestBody ResetPasswordDto resetPassword) {
        userService.resetPassword(resetPassword.getToken(), resetPassword.getPassword());
        return ResponseEntity.ok(PASSWORD_WAS_RESET_SUCCESSFULLY);
    }

    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER')")
    @GetMapping(path = "/email-update")
    public ResponseEntity<String> emailUpdate(@RequestParam(value = "token") @NotEmpty String token) {
        userService.updateEmailOfAccountOwner(token);
        return ResponseEntity.ok(EMAIL_WAS_SUCCESSFULLY_UPDATED);
    }
}
