package ru.itterminal.yanmas.tickets.model.dto;


import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.commons.model.dto.BaseEntityDto;
import ru.itterminal.yanmas.commons.model.validator.enums.ValueOfEnum;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.commons.model.validator.scenario.Update;
import ru.itterminal.yanmas.tickets.model.Priority;

import java.util.List;
import java.util.UUID;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class TicketEventDtoRequest  extends BaseEntityDto {

    private String comment;
    private UUID ticketId;
    private Boolean isCommentForExecutor;
    private UUID newAuthorId;
    private String newSubject;
    private String newDescription;
    @ValueOfEnum(enumClass = Priority.class, message = "must be any of: low, middle, height", groups = {Create.class, Update.class})
    private String newPriority;
    private Long newDeadline;
    private Boolean newIsFinished;
    private UUID newTicketTypeId;
    private UUID newTicketStatusId;
    private List<UUID> newObservers;
    private List<UUID> newExecutors;
}
