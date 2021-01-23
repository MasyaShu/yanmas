package ru.itterminal.botdesk.tickets.model.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;
import ru.itterminal.botdesk.commons.model.filter.StringFilter;
import ru.itterminal.botdesk.commons.model.validator.ValidSortFields;
import ru.itterminal.botdesk.commons.model.validator.ValidateFilter;

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

    @ValidSortFields(sortFields = "deleted, displayName, name, sortIndex")
    private List<String> sortByFields;
}
