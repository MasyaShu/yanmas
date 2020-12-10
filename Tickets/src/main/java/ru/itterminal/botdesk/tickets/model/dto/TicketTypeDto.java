package ru.itterminal.botdesk.tickets.model.dto;

import lombok.*;
import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Null;
import javax.validation.constraints.Size;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class TicketTypeDto extends BaseEntityDto {

    @NotNull(groups = {Create.class, Update.class})
    @Size(min = 1, max = 128, groups = {Create.class, Update.class})
    private String name;

    private String comment;

    @Null(groups = {Create.class, Update.class})
    private Boolean isPredefined;

}
