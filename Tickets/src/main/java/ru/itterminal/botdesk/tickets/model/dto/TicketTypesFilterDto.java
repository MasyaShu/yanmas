package ru.itterminal.botdesk.tickets.model.dto;

import lombok.*;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;
import ru.itterminal.botdesk.commons.model.validator.ValueOfEnum;

import javax.validation.constraints.Size;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class TicketTypesFilterDto extends BaseFilterDto {

    @Size(min = 1, max = 128)
    private String name;

    private String comment;

    @ValueOfEnum(enumClass = FieldsForSort.class, message = "must be any of: name")
    private String sortBy = "name";

    @SuppressWarnings("unused")
    public enum FieldsForSort {
        NAME
    }

}
