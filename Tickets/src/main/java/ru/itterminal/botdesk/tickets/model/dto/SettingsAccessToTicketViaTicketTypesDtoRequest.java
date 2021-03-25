package ru.itterminal.botdesk.tickets.model.dto;

import java.util.List;
import java.util.UUID;

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
//@NoArgsConstructor
@ToString
@EqualsAndHashCode(callSuper = true)
public class SettingsAccessToTicketViaTicketTypesDtoRequest extends BaseEntityDto {

//    private UUID authorId;
//
//    private UUID groupId;
//
//    List<UUID> observers;
//
//    List<UUID> executors;
//
//    private UUID ticketTypeForNewId;
//
//    private UUID ticketStatusForNewId;
//
//    private UUID ticketStatusForReopenId;
//
//    private UUID ticketStatusForCloseId;
//
//    private UUID ticketStatusForCancelId;
}
