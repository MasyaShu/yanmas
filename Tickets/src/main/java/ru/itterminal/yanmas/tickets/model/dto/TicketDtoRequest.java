package ru.itterminal.yanmas.tickets.model.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.commons.model.dto.BaseEntityDto;
import ru.itterminal.yanmas.commons.model.validator.enums.ValueOfEnum;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.commons.model.validator.scenario.Update;
import ru.itterminal.yanmas.tickets.model.Priority;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Null;
import javax.validation.constraints.Size;
import java.util.List;
import java.util.UUID;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class TicketDtoRequest extends BaseEntityDto {
    @NotNull(groups = {Create.class, Update.class})
    private UUID authorId;
    @Size(max = 256, groups = {Create.class, Update.class})
    private String subject;
    @ValueOfEnum(enumClass = Priority.class, message = "must be any of: low, middle, height", groups = {Create.class, Update.class})
    private String priority;
    private String description;
    private Long deadline;
    private Boolean isFinished;
    private UUID ticketTypeId;
    private UUID ticketStatusId;
    private List<UUID> observers;
    private List<UUID> executors;
    @Null(groups = {Update.class})
    private List<UUID> files;
}
