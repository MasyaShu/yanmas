package ru.itterminal.yanmas.aau.model.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.aau.util.AAUConstants;
import ru.itterminal.yanmas.commons.model.dto.BaseFilterDto;
import ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter;
import ru.itterminal.yanmas.commons.model.filter.BooleanFilter;
import ru.itterminal.yanmas.commons.model.filter.StringFilter;
import ru.itterminal.yanmas.commons.model.validator.sortfields.ValidateSortFields;
import ru.itterminal.yanmas.commons.model.validator.filter.ValidateFilter;

import java.util.List;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class UserFilterDto extends BaseFilterDto {

    @ValidateFilter(regexp = AAUConstants.EMAIL_PATTERN, messageRegexp = AAUConstants.INVALID_EMAIL)
    private StringFilter email;

    @ValidateFilter(max = 128)
    private StringFilter name;

    @ValidateFilter(min = 6, max = 128)
    private StringFilter phone;

    @ValidateFilter
    private StringFilter comment;

    @ValidateFilter
    private BooleanFilter isArchived;

    @ValidateFilter
    private BaseEntityFilter group;

    @ValidateFilter
    private BaseEntityFilter role;

    @ValidateSortFields(permittedFieldsForSort = "deleted, displayName, email, " +
            "name, phone, comment, isArchived")
    private List<String> sortByFields;
}
