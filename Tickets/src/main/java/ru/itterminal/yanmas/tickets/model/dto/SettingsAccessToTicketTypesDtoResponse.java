package ru.itterminal.yanmas.tickets.model.dto;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.commons.model.dto.BaseEntityDto;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
@EqualsAndHashCode(callSuper = true)
public class SettingsAccessToTicketTypesDtoResponse extends BaseEntityDto {
    private BaseEntityDto group;
    private BaseEntityDto user;
    private BaseEntityDto groupTicketTypes;
}
