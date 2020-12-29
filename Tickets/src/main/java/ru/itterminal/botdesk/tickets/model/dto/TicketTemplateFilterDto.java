package ru.itterminal.botdesk.tickets.model.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;

import java.util.List;
import java.util.UUID;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class TicketTemplateFilterDto extends BaseFilterDto {

    private String subject;

    private String description;

    private Long dateStart;

    private String comparisonDataEnd;

    private Long dateEnd;

    private String comparisonDataStart;

    private Boolean isOnlyOneTicketInWork;

    private Boolean isActive;

    private List<UUID> authorId;

    private List<UUID> ticketTypeId;
}
