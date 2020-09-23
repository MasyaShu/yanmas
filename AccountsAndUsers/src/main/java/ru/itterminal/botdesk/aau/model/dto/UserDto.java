package ru.itterminal.botdesk.aau.model.dto;

import static ru.itterminal.botdesk.aau.util.AAUConstants.INVALID_EMAIL;
import static ru.itterminal.botdesk.aau.util.AAUConstants.INVALID_PASSWORD;
import static ru.itterminal.botdesk.aau.util.AAUConstants.MUST_BE_ANY_OF_EN_RU;
import static ru.itterminal.botdesk.aau.util.AAUConstants.emailPattern;
import static ru.itterminal.botdesk.aau.util.AAUConstants.passwordPattern;

import java.util.Set;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
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
import ru.itterminal.botdesk.aau.model.Language;
import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;
import ru.itterminal.botdesk.commons.model.validator.ValueOfEnum;
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

    @Size(max = 20, groups = {Create.class, Update.class})
    private String firstName;

    @Size(max = 30, groups = {Create.class, Update.class})
    private String secondName;

    @NotNull(groups = {Create.class, Update.class})
    @Pattern(regexp = passwordPattern,
            message = INVALID_PASSWORD,
            groups = {Create.class, Update.class})
    private String password;

    @Size(max = 30, groups = {Create.class, Update.class})
    private String phone;

    private String comment;

    @ValueOfEnum(enumClass = Language.class,
            message = MUST_BE_ANY_OF_EN_RU,
            groups = {Create.class, Update.class})
    private String language;

    @NotNull(groups = {Create.class, Update.class})
    private Boolean isArchived;

    @NotNull(groups = {Create.class, Update.class})
    private Account account;

    @NotNull(groups = {Create.class, Update.class})
    private Group group;

    @NotEmpty(groups = {Create.class, Update.class})
    private Set<Role> roles;
}