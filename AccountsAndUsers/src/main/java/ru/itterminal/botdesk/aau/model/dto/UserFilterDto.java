package ru.itterminal.botdesk.aau.model.dto;

import static ru.itterminal.botdesk.aau.util.AAUConstants.INVALID_EMAIL;
import static ru.itterminal.botdesk.aau.util.AAUConstants.emailPattern;

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
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;

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
