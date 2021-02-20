package ru.itterminal.botdesk.tickets.model.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;
import ru.itterminal.botdesk.commons.model.filter.StringFilter;
import ru.itterminal.botdesk.commons.model.validator.filter.ValidateFilter;
import ru.itterminal.botdesk.commons.model.validator.sortfields.ValidateSortFields;

import java.util.List;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class TicketTypeFilterDto extends BaseFilterDto {

    @ValidateFilter(min = 1, max = 128)
    private StringFilter name;

    @ValidateFilter
    private StringFilter comment;

    @ValidateSortFields(permittedFieldsForSort = "deleted, displayName, name, comment")
    private List<String> sortByFields;
}
