package ru.itterminal.yanmas.aau.model.dto;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.aau.util.AAUConstants;
import ru.itterminal.yanmas.commons.model.dto.BaseEntityDto;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;

@Getter
@Setter
@SuperBuilder
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
    @Pattern(regexp = AAUConstants.EMAIL_PATTERN,
            message = AAUConstants.INVALID_EMAIL,
            groups = {Create.class})
    private String emailAccountOwner;

    @NotNull(groups = {Create.class})
    @Pattern(regexp = AAUConstants.PASSWORD_PATTERN,
            message = AAUConstants.INVALID_PASSWORD,
            groups = {Create.class})
    private String passwordAccountOwner;
}
