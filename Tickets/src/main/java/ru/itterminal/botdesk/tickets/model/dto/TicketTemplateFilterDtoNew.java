package ru.itterminal.botdesk.tickets.model.dto;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDtoNew;
import ru.itterminal.botdesk.commons.model.filter.BaseEntityFilter;
import ru.itterminal.botdesk.commons.model.filter.BooleanFilter;
import ru.itterminal.botdesk.commons.model.filter.NumberFilter;
import ru.itterminal.botdesk.commons.model.filter.StringFilter;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class TicketTemplateFilterDtoNew extends BaseFilterDtoNew {

    private StringFilter subject;

    private StringFilter description;

    private NumberFilter dateStart;

    private StringFilter comparisonDataEnd;

    private NumberFilter dateEnd;

    private StringFilter comparisonDataStart;

    private BooleanFilter isOnlyOneTicketInWork;

    private BooleanFilter isActive;

    private BaseEntityFilter author;

    private BaseEntityFilter ticketType;
}
