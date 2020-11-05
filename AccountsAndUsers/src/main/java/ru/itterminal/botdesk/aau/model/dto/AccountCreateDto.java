package ru.itterminal.botdesk.aau.model.dto;

import static ru.itterminal.botdesk.aau.util.AAUConstants.INVALID_EMAIL;
import static ru.itterminal.botdesk.aau.util.AAUConstants.INVALID_PASSWORD;
import static ru.itterminal.botdesk.aau.util.AAUConstants.EMAIL_PATTERN;
import static ru.itterminal.botdesk.aau.util.AAUConstants.PASSWORD_PATTERN;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class AccountCreateDto extends BaseEntityDto {
    @NotNull(groups = {Create.class})
    @Size(min = 3, max = 128, groups = {Create.class})
    private String name;

    @NotNull(groups = {Create.class})
    @Size(min = 1, max = 128, groups = {Create.class})
    private String nameGroupAccountOwner;

    @NotNull(groups = {Create.class})
    @Pattern(regexp = EMAIL_PATTERN,
            message = INVALID_EMAIL,
            groups = {Create.class})
    private String emailAccountOwner;

    @NotNull(groups = {Create.class})
    @Pattern(regexp = PASSWORD_PATTERN,
            message = INVALID_PASSWORD,
            groups = {Create.class})
    private String passwordAccountOwner;
}
