package ru.itterminal.botdesk.aau.model.dto;

import static ru.itterminal.botdesk.commons.util.CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY;

import java.util.UUID;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Null;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;
import ru.itterminal.botdesk.aau.util.AAUConstants;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class UserDto extends BaseEntityDto {

    @NotNull(groups = {Create.class, Update.class})
    // TODO Для валидации email есть аннотация @Email javax.validation.constraints.Email
    @Pattern(regexp = AAUConstants.EMAIL_PATTERN,
            message = AAUConstants.INVALID_EMAIL,
            groups = {Create.class, Update.class})
    private String email;

    @Size(min = 1, max = 20, groups = {Create.class, Update.class})
    private String firstName;

    @Size(min = 1, max = 30, groups = {Create.class, Update.class})
    private String secondName;

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
    private UUID groupId;

    @NotNull(groups = {Create.class, Update.class})
    private UUID roleId;

}
