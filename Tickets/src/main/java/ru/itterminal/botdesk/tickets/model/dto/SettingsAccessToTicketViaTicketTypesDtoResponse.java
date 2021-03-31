package ru.itterminal.botdesk.tickets.model.dto;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
@EqualsAndHashCode(callSuper = true)
public class SettingsAccessToTicketViaTicketTypesDtoResponse extends BaseEntityDto {
    private BaseEntityDto group;
    private BaseEntityDto user;
    private BaseEntityDto groupTicketTypes;
}
