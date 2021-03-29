package ru.itterminal.botdesk.tickets.model.dto;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;
import ru.itterminal.botdesk.commons.model.filter.BaseEntityFilter;
import ru.itterminal.botdesk.commons.model.validator.filter.ValidateFilter;
import ru.itterminal.botdesk.commons.model.validator.sortfields.ValidateSortFields;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class SettingsAccessToTicketViaTicketTypesFilterDto extends BaseFilterDto {

    @ValidateFilter
    private BaseEntityFilter group;

    @ValidateFilter
    private BaseEntityFilter user;

    @ValidateFilter
    private BaseEntityFilter groupTicketTypes;

    @ValidateSortFields(permittedFieldsForSort = "deleted, displayName")
    private List<String> sortByFields;

}
