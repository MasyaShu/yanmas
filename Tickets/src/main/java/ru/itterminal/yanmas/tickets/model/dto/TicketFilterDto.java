package ru.itterminal.yanmas.tickets.model.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.commons.model.dto.BaseFilterDto;
import ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter;
import ru.itterminal.yanmas.commons.model.filter.BooleanFilter;
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
public class TicketFilterDto extends BaseFilterDto {

    @ValidateFilter
    private BaseEntityFilter author;

    @ValidateFilter
    private BaseEntityFilter group;

    @ValidateFilter
    private NumberFilter number;

    @ValidateFilter
    private NumberFilter createdAt;

    @ValidateFilter
    private StringFilter subject;

    @ValidateFilter
    private StringFilter description;

    @ValidateFilter
    private NumberFilter deadline;

    @ValidateFilter
    private BooleanFilter isFinished;

    @ValidateFilter
    private BaseEntityFilter ticketType;

    @ValidateFilter
    private BaseEntityFilter ticketStatus;

    @ValidateFilter
    private BaseEntityFilter ticketTemplate;

    @ValidateFilter
    private ListOfBaseEntityFilter observers;

    @ValidateFilter
    private ListOfBaseEntityFilter executors;

    @ValidateFilter
    private ListOfBaseEntityFilter files;

    @ValidateSortFields(permittedFieldsForSort = "deleted, displayName, isFinished, " +
            "createdAt, deadline,  subject, description, number")
    private List<String> sortByFields;
}
