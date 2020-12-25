package ru.itterminal.botdesk.tickets.model.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;
import ru.itterminal.botdesk.tickets.util.TicketConstants;

import javax.validation.constraints.Pattern;
import java.util.List;
import java.util.UUID;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class TicketTemplateFilterDto extends BaseEntityDto {

    private String subject;

    private String description;

    private Long dateStart;

    @Pattern(regexp = TicketConstants.COMPARISON_PATTERN,
            message = TicketConstants.INVALID_COMPARISON)
    private String comparisonDataEnd;

    private Long dateEnd;

    @Pattern(regexp = TicketConstants.COMPARISON_PATTERN,
            message = TicketConstants.INVALID_COMPARISON)
    private String comparisonDataStart;

    private Boolean isOnlyOneTicketInWork;

    private Boolean isActive;

    private List<UUID> authorId;

    private List<UUID> ticketTypeId;
}
