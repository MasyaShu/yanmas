package ru.itterminal.botdesk.tickets.model.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;
import ru.itterminal.botdesk.commons.model.filter.BaseEntityFilter;
import ru.itterminal.botdesk.commons.model.filter.BooleanFilter;
import ru.itterminal.botdesk.commons.model.filter.NumberFilter;
import ru.itterminal.botdesk.commons.model.filter.StringFilter;
import ru.itterminal.botdesk.commons.model.validator.sortfields.ValidateSortFields;
import ru.itterminal.botdesk.commons.model.validator.filter.ValidateFilter;

import java.util.List;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class TicketTemplateFilterDto extends BaseFilterDto {

    @ValidateFilter(min = 7)
    private StringFilter subject;

    @ValidateFilter()
    private StringFilter description;

    @ValidateFilter()
    private NumberFilter dateStart;

    @ValidateFilter()
    private NumberFilter dateEnd;

    @ValidateFilter()
    private BooleanFilter isOnlyOneTicketInWork;

    @ValidateFilter()
    private BooleanFilter isActive;

    @ValidateFilter()
    private BaseEntityFilter author;

    @ValidateFilter()
    private BaseEntityFilter ticketType;

    @ValidateSortFields(permittedFieldsForSort = "deleted, displayName, subject, " +
            "description, dateStart, dateEnd, isOnlyOneTicketInWork, " +
            "isActive, zoneId, dateNextRun")
    private List<String> sortByFields;

}
