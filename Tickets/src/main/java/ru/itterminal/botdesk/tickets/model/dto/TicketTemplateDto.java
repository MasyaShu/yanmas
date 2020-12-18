package ru.itterminal.botdesk.tickets.model.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.BaseEntity;
import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;
import ru.itterminal.botdesk.commons.model.validator.Scheduler;
import ru.itterminal.botdesk.commons.model.validator.ZoneId;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Null;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class TicketTemplateDto extends BaseEntityDto {

    private String subject;

    private String description;

    @Null(groups = {Create.class, Update.class})
    private Long dateNextRun;

    private Long dateStart;

    private Long dateEnd;

    @ZoneId(groups = {Create.class, Update.class})
    @NotNull(groups = {Create.class, Update.class})
    private String zoneId;

    @Scheduler(groups = {Create.class, Update.class})
    @NotNull(groups = {Create.class, Update.class})
    private String expressionSchedule;

    @NotNull(groups = {Create.class, Update.class})
    private Boolean isOnlyOneTicketInWork;

    @NotNull(groups = {Create.class, Update.class})
    private Boolean isActive;

    @NotNull(groups = {Create.class, Update.class})
    private BaseEntity Author;

    @NotNull(groups = {Create.class, Update.class})
    private BaseEntity ticketType;
}
