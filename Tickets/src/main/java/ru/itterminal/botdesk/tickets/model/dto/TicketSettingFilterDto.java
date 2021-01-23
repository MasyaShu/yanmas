package ru.itterminal.botdesk.tickets.model.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;
import ru.itterminal.botdesk.commons.model.validator.ValidSortFields;

import java.util.List;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@ToString
public class TicketSettingFilterDto extends BaseFilterDto {

    private static final String SORT_FIELDS = "deleted, displayName";

    @ValidSortFields(sortFields = SORT_FIELDS)
    private List<String> sortByFields;

}
