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
    private UUID ticketId;
    private String comment;
    private String autoComment;
    private Boolean isCommentForExecutor;
    private List<UUID> files;
}
