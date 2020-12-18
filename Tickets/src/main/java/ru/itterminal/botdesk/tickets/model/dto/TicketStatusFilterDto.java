package ru.itterminal.botdesk.tickets.model.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;

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
}
