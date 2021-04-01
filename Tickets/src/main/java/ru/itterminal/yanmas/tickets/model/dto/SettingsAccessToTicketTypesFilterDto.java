package ru.itterminal.yanmas.tickets.model.dto;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.commons.model.dto.BaseFilterDto;
import ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter;
import ru.itterminal.yanmas.commons.model.validator.filter.ValidateFilter;
import ru.itterminal.yanmas.commons.model.validator.sortfields.ValidateSortFields;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class SettingsAccessToTicketTypesFilterDto extends BaseFilterDto {

    @ValidateFilter
    private BaseEntityFilter group;

    @ValidateFilter
    private BaseEntityFilter user;

    @ValidateFilter
    private BaseEntityFilter groupTicketTypes;

    @ValidateSortFields(permittedFieldsForSort = "deleted, displayName")
    private List<String> sortByFields;

}
