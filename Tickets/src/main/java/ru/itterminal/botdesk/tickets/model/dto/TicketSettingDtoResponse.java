package ru.itterminal.botdesk.tickets.model.dto;

import java.util.List;

import javax.validation.constraints.NotNull;

import lombok.AllArgsConstructor;
import lombok.Getter;
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
@ToString
public class TicketSettingDtoResponse extends BaseEntityDto {

    @NotNull(groups = {Create.class, Update.class})
    private BaseEntityDto authorId;

    @NotNull(groups = {Create.class, Update.class})
    private BaseEntityDto groupId;

    List<BaseEntityDto> observersId;

    List<BaseEntityDto> executorsId;

    private BaseEntityDto ticketTypeIdForNew;

    private BaseEntityDto ticketStatusIdForNew;

    private BaseEntityDto ticketStatusIdForReopen;

    private BaseEntityDto ticketStatusIdForClose;

    private BaseEntityDto ticketStatusIdForCancel;
}
