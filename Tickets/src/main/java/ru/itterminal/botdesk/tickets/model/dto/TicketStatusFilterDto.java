package ru.itterminal.botdesk.tickets.model.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;
import ru.itterminal.botdesk.commons.model.filter.StringFilter;
import ru.itterminal.botdesk.commons.model.validator.sortfields.ValidateSortFields;
import ru.itterminal.botdesk.commons.model.validator.filter.ValidateFilter;

import java.util.List;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class TicketStatusFilterDto  extends BaseFilterDto {

    @ValidateFilter(min = 1, max = 128)
    private StringFilter name;

    @ValidateSortFields(permittedFieldsForSort = "deleted, displayName, name, sortIndex")
    private List<String> sortByFields;
}
