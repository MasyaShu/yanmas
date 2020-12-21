package ru.itterminal.botdesk.tickets.model.dto;

import java.util.List;

import javax.validation.constraints.NotNull;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class TicketSettingDtoResponse extends BaseEntityDto {

    private BaseEntityDto author;

    private BaseEntityDto group;

    List<BaseEntityDto> observers;

    List<BaseEntityDto> executors;

    private BaseEntityDto ticketTypeForNew;

    private BaseEntityDto ticketStatusForNew;

    private BaseEntityDto ticketStatusForReopen;

    private BaseEntityDto ticketStatusForClose;

    private BaseEntityDto ticketStatusForCancel;

}
