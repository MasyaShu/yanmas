package ru.itterminal.botdesk.aau.model.dto;

import lombok.*;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Delete;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Null;
import javax.validation.constraints.Size;

import static ru.itterminal.botdesk.commons.util.CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class GroupDto extends BaseEntityDto {

    @NotNull(groups = {Create.class, Update.class, Delete.class})
    @Size(min = 1, max = 128, groups = {Create.class, Update.class})
    private String name;

    private String comment;

    @NotNull(groups = {Create.class, Update.class})
    private Boolean isInner;

    @Null(groups = Create.class, message = MUST_BE_NULL_FOR_THE_NEW_ENTITY)
    @NotNull(groups = {Update.class, Delete.class})
    private Boolean isDeprecated;
}
