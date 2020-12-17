package ru.itterminal.botdesk.tickets.model.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@ToString
public class TicketSettingFilterDto extends BaseFilterDto {

}
