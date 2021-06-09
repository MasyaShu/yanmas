package ru.itterminal.yanmas.tickets.model.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.commons.model.dto.BaseFilterDto;
import ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter;
import ru.itterminal.yanmas.commons.model.filter.ListOfBaseEntityFilter;
import ru.itterminal.yanmas.commons.model.filter.NumberFilter;
import ru.itterminal.yanmas.commons.model.filter.StringFilter;
import ru.itterminal.yanmas.commons.model.validator.filter.ValidateFilter;
import ru.itterminal.yanmas.commons.model.validator.sortfields.ValidateSortFields;

import java.util.List;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class TicketEventFilterDto  extends BaseFilterDto {

    @ValidateFilter
    private StringFilter comment;

    @ValidateFilter
    private StringFilter autoComment;

    @ValidateFilter
    private NumberFilter createdAt;

    @ValidateFilter
    private BaseEntityFilter ticket;

    @ValidateFilter
    private BaseEntityFilter createdBy;

    @ValidateFilter
    private ListOfBaseEntityFilter files;

    @ValidateFilter
    private ListOfBaseEntityFilter recipients;

    @ValidateSortFields(permittedFieldsForSort = "deleted, displayName, recipients, " +
            "createdAt, createdBy,  comment, autoComment")
    private List<String> sortByFields;

}
