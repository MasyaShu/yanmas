package ru.itterminal.botdesk.tickets.model.dto;

import javax.validation.constraints.NotNull;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
@EqualsAndHashCode(callSuper = true)
public class TicketCounterDto extends BaseEntityDto {

    @NotNull(groups = {Update.class})
    private Long currentNumber;

}
