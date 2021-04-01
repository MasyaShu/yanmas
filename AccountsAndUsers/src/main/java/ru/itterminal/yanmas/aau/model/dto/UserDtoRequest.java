package ru.itterminal.yanmas.aau.model.dto;

import static ru.itterminal.yanmas.commons.util.CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY;

import java.util.UUID;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Null;
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
import ru.itterminal.yanmas.commons.model.validator.scenario.Update;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class UserDtoRequest extends BaseEntityDto {

    @NotNull(groups = {Create.class, Update.class})
    @Pattern(regexp = AAUConstants.EMAIL_PATTERN,
            message = AAUConstants.INVALID_EMAIL,
            groups = {Create.class, Update.class})
    private String email;

    @Size(max = 128, groups = {Create.class, Update.class})
    private String name;

    @NotNull(groups = {Create.class})
    @Pattern(regexp = AAUConstants.PASSWORD_PATTERN,
            message = AAUConstants.INVALID_PASSWORD,
            groups = {Create.class, Update.class})
    private String password;

    @Size(min = 6, max = 30, groups = {Create.class, Update.class})
    private String phone;

    private String comment;

    @Null(groups = Create.class,
            message = MUST_BE_NULL_FOR_THE_NEW_ENTITY)
    @NotNull(groups = {Update.class})
    private Boolean isArchived;

    @NotNull(groups = {Create.class, Update.class})
    private UUID group;

    @NotNull(groups = {Create.class, Update.class})
    private UUID role;

}
