package ru.itterminal.yanmas.tickets.model.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.commons.model.dto.BaseEntityDto;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.commons.model.validator.scenario.Update;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Null;
import javax.validation.constraints.Size;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
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
