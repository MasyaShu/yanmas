package ru.itterminal.botdesk.tickets.model.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;
import ru.itterminal.botdesk.commons.model.validator.scheduler.ValidateScheduler;
import ru.itterminal.botdesk.commons.model.validator.zoneid.ValidateZoneId;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;

import javax.validation.constraints.NotNull;
import java.util.UUID;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class TicketTemplateDtoRequest extends BaseEntityDto {

    @NotNull(groups = {Create.class, Update.class})
    private String subject;

    private String description;

    private Long dateStart;

    private Long dateEnd;

    @ValidateZoneId(groups = {Create.class, Update.class})
    private String zoneId;

    @ValidateScheduler(groups = {Create.class, Update.class})
    private String expressionSchedule;

    @NotNull(groups = {Create.class, Update.class})
    private Boolean isOnlyOneTicketInWork;

    @NotNull(groups = {Create.class, Update.class})
    private Boolean isActive;

    @NotNull(groups = {Create.class, Update.class})
    private UUID authorId;

    private UUID ticketTypeId;

}
