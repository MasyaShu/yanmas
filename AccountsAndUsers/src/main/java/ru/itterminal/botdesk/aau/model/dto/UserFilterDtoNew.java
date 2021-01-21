package ru.itterminal.botdesk.aau.model.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDtoNew;
import ru.itterminal.botdesk.commons.model.filter.BaseEntityFilter;
import ru.itterminal.botdesk.commons.model.filter.BooleanFilter;
import ru.itterminal.botdesk.commons.model.filter.StringFilter;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class UserFilterDtoNew extends BaseFilterDtoNew {

    //@Pattern(regexp = AAUConstants.EMAIL_PATTERN, message = AAUConstants.INVALID_EMAIL)
    private StringFilter email;

    //@Size(max = 128)
    private StringFilter name;

    //@Size(min = 6, max = 30)
    private StringFilter phone;

    private StringFilter comment;

    private BooleanFilter isArchived;

    private BaseEntityFilter group;

    private BaseEntityFilter role;
}
