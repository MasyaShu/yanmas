package ru.itterminal.botdesk.tickets.model.dto;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;
import ru.itterminal.botdesk.commons.model.filter.ListOfBaseEntityFilter;
import ru.itterminal.botdesk.commons.model.filter.StringFilter;
import ru.itterminal.botdesk.commons.model.validator.filter.ValidateFilter;
import ru.itterminal.botdesk.commons.model.validator.sortfields.ValidateSortFields;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class GroupTicketTypesFilterDto extends BaseFilterDto {

    @ValidateFilter(min = 1, max = 256)
    private StringFilter name;

    @ValidateFilter
    private ListOfBaseEntityFilter ticketTypes;

    @ValidateSortFields(permittedFieldsForSort = "deleted, displayName")
    private List<String> sortByFields;
}
