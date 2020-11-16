package ru.itterminal.botdesk.aau.model.dto;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import ru.itterminal.botdesk.aau.util.AAUConstants;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class ResetPasswordDto {

    @NotNull
    private String token;

    @NotNull
    @Pattern(regexp = AAUConstants.PASSWORD_PATTERN,
            message = AAUConstants.INVALID_PASSWORD)
    private String password;
}
