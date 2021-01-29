package ru.itterminal.botdesk.tickets.model.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;
import ru.itterminal.botdesk.commons.model.filter.BaseEntityFilter;
import ru.itterminal.botdesk.commons.model.filter.BooleanFilter;
import ru.itterminal.botdesk.commons.model.filter.ListOfBaseEntityFilter;
import ru.itterminal.botdesk.commons.model.filter.NumberFilter;
import ru.itterminal.botdesk.commons.model.filter.StringFilter;
import ru.itterminal.botdesk.commons.model.validator.sortfields.ValidateSortFields;

import java.util.List;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class TicketFilterDto extends BaseFilterDto {

    private BaseEntityFilter author;

    private NumberFilter number;

    private NumberFilter createdAt;

    private StringFilter subject;

    private StringFilter description;

    private NumberFilter deadline;

    private BooleanFilter isFinished;

    private BaseEntityFilter ticketType;

    private BaseEntityFilter ticketStatus;

    private BaseEntityFilter ticketTemplate;

    private BaseEntityFilter ticketInheritor;

    private ListOfBaseEntityFilter observers;

    private ListOfBaseEntityFilter executors;

    private ListOfBaseEntityFilter files;

    @ValidateSortFields(permittedFieldsForSort = "deleted, displayName, isFinished, " +
            "createdAt, deadline,  subject, description, number")
    private List<String> sortByFields;
}