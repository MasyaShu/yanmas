package ru.itterminal.botdesk.tickets.model.dto;

import java.util.List;
import java.util.UUID;

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
public class TicketSettingDtoRequest extends BaseEntityDto {

    @NotNull(groups = {Create.class, Update.class})
    private UUID authorId;

    @NotNull(groups = {Create.class, Update.class})
    private UUID groupId;

    List<UUID> observersId;

    List<UUID> executorsId;

    private UUID ticketTypeIdForNew;

    private UUID ticketStatusIdForNew;

    private UUID ticketStatusIdForReopen;

    private UUID ticketStatusIdForClose;

    private UUID ticketStatusIdForCancel;
}
