package ru.itterminal.botdesk.tickets.model.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;
import ru.itterminal.botdesk.commons.model.validator.ValueOfEnum;

import javax.validation.constraints.Size;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class TicketStatusFilterDto  extends BaseFilterDto {

    @Size(min = 1, max = 128)
    private String name;

    @ValueOfEnum(enumClass = TicketTypeFilterDto.FieldsForSort.class, message = "must be any of: sortIndex")
    private String sortBy = "sortIndex";

    @SuppressWarnings("unused")
    public enum FieldsForSort {
        SORT_INDEX
    }
}
