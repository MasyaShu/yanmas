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
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;
import ru.itterminal.botdesk.commons.model.validator.ValueOfEnum;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class UserFilterDto extends BaseFilterDto {

    @Pattern(regexp = emailPattern,
            message = INVALID_EMAIL)
    private String email;

    @Size(max = 20)
    private String firstName;

    @Size(max = 30)
    private String secondName;

    @Size(max = 30)
    private String phone;

    private String comment;

    private Boolean isArchived;

    private Account account;

    private Group group;

}
