package ru.itterminal.botdesk.aau.model.dto;

import static ru.itterminal.botdesk.commons.service.UserCommons.INVALID_EMAIL;
import static ru.itterminal.botdesk.commons.service.UserCommons.INVALID_PASSWORD;
import static ru.itterminal.botdesk.commons.service.UserCommons.emailPattern;
import static ru.itterminal.botdesk.commons.service.UserCommons.passwordPattern;

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
public class AccountDto extends BaseEntityDto {

    @NotNull(groups = {Create.class, Update.class})
    @ValueOfEnum(enumClass = Language.class,
            message = "must be any of: en, ru",
            groups = {Create.class, Update.class})
    private String language;
}
