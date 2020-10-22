package ru.itterminal.botdesk.aau.model.dto;

import lombok.*;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class GroupDto extends BaseEntityDto {

    @Size(min = 1, max = 128, groups = {Create.class, Update.class})
    private String name;

    private String comment;

    @NotNull(groups = {Create.class, Update.class})
    private Boolean isInner;

    @NotNull(groups = {Create.class, Update.class})
    private Boolean isDeprecated;

    @NotNull(groups = {Create.class, Update.class})
    private Account account;

}
