package ru.itterminal.botdesk.tickets.model.dto;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.validation.constraints.NotNull;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;
import ru.itterminal.botdesk.tickets.model.TicketStatus;
import ru.itterminal.botdesk.tickets.model.TicketType;

@Getter
@Setter
@Builder
@AllArgsConstructor
@ToString
public class TicketSettingDto extends BaseEntityDto {

    @NotNull(groups = {Create.class, Update.class})
    private UUID accountId;

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
