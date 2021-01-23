package ru.itterminal.botdesk.tickets.model.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;
import ru.itterminal.botdesk.commons.model.validator.ValidSortFields;

import javax.validation.constraints.Size;
import java.util.List;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class TicketTypeFilterDto extends BaseFilterDto {

    private static final String SORT_FIELDS = "deleted, displayName, name, comment";

    @Size(min = 1, max = 128)
    private String name;

    private String comment;

    @ValidSortFields(sortFields = SORT_FIELDS)
    private List<String> sortByFields;
}
