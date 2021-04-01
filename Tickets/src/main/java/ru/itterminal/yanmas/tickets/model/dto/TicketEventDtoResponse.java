package ru.itterminal.yanmas.tickets.model.dto;


import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.commons.model.dto.BaseEntityDto;

import java.util.List;
import java.util.UUID;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class TicketEventDtoResponse extends BaseEntityDto {

    private String comment;
    private UUID ticketId;
    private UUID newAuthorId;
    private String newSubject;
    private String newDescription;
    private Long newDeadline;
    private Boolean newIsFinished;
    private UUID newTicketTypeId;
    private UUID newTicketStatusId;
    private List<UUID> newObservers;
    private List<UUID> newExecutors;
}
