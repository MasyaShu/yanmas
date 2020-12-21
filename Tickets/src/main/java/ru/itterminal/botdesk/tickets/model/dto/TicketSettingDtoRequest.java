package ru.itterminal.botdesk.tickets.model.dto;

import java.util.List;
import java.util.UUID;

import javax.validation.constraints.NotNull;

import lombok.AllArgsConstructor;
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
@ToString
public class TicketSettingDtoRequest extends BaseEntityDto {

    @NotNull(groups = {Create.class, Update.class})
    private UUID author;

    @NotNull(groups = {Create.class, Update.class})
    private UUID group;

    List<UUID> observers;

    List<UUID> executors;

    private UUID ticketTypeForNew;

    private UUID ticketStatusForNew;

    private UUID ticketStatusForReopen;

    private UUID ticketStatusForClose;

    private UUID ticketStatusForCancel;
}
