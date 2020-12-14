package ru.itterminal.botdesk.tickets.model.dto;

import lombok.*;
import org.springframework.scheduling.annotation.Scheduled;
import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;
import ru.itterminal.botdesk.commons.model.validator.Scheduler;
import ru.itterminal.botdesk.commons.model.validator.ZoneId;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Null;
import java.util.UUID;

@Getter
@Setter
@Builder
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
    private UUID AuthorId;

    @NotNull(groups = {Create.class, Update.class})
    private UUID ticketTypeId;
}
