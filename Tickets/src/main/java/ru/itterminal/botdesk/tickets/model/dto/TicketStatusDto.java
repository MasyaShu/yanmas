package ru.itterminal.botdesk.tickets.model.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Null;
import javax.validation.constraints.Size;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class TicketStatusDto  extends BaseEntityDto {

    @NotNull(groups = {Create.class, Update.class})
    @Size(min = 1, max = 128, groups = {Create.class, Update.class})
    private String name;

    @NotNull(groups = {Create.class, Update.class})
    private Integer sortIndex;

    @Null(groups = {Create.class, Update.class})
    private Boolean isStartedPredefined;

    @Null(groups = {Create.class, Update.class})
    private Boolean isFinishedPredefined;

    @Null(groups = {Create.class, Update.class})
    private Boolean isReopenedPredefined;

    @Null(groups = {Create.class, Update.class})
    private Boolean isCanceledPredefined;

}
