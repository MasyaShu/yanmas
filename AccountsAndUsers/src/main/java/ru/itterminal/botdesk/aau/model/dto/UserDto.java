package ru.itterminal.botdesk.aau.model.dto;

import static ru.itterminal.botdesk.aau.util.AAUConstants.INVALID_EMAIL;
import static ru.itterminal.botdesk.aau.util.AAUConstants.INVALID_PASSWORD;
import static ru.itterminal.botdesk.aau.util.AAUConstants.emailPattern;
import static ru.itterminal.botdesk.aau.util.AAUConstants.passwordPattern;
import static ru.itterminal.botdesk.commons.util.CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY;

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
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class UserDto extends BaseEntityDto {

    @NotNull(groups = {Create.class, Update.class})
    @Pattern(regexp = emailPattern,
            message = INVALID_EMAIL,
            groups = {Create.class, Update.class})
    private String email;

    @Size(min = 1, max = 20, groups = {Create.class, Update.class})
    private String firstName;

    @Size(min = 1, max = 30, groups = {Create.class, Update.class})
    private String secondName;

    @NotNull(groups = {Create.class, Update.class})
    @Pattern(regexp = passwordPattern,
            message = INVALID_PASSWORD,
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
    private Account account;

    @NotNull(groups = {Create.class, Update.class})
    private Group group;

    @NotNull(groups = {Create.class, Update.class})
    private Role role;

}
