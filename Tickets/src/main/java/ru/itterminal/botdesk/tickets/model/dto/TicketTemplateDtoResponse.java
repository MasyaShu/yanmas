package ru.itterminal.botdesk.tickets.model.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.BaseEntity;
import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class TicketTemplateDtoResponse extends BaseEntityDto {

    private String subject;

    private String description;

    private Long dateNextRun;

    private Long dateStart;

    private Long dateEnd;

    private String zoneId;

    private String expressionSchedule;

    private Boolean isOnlyOneTicketInWork;

    private Boolean isActive;

    private BaseEntity author;

    private BaseEntity ticketType;
}
