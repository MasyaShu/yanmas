package ru.itterminal.yanmas.tickets.model.dto;

import java.util.List;
import java.util.UUID;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.commons.model.dto.BaseEntityDto;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class TicketDtoRequest extends BaseEntityDto {
    @NotNull(groups = {Create.class})
    private UUID authorId;
    @Size(max = 256, groups = {Create.class})
    private String subject;
    private String description;
    private Long deadline;
    private Boolean isFinished;
    private UUID ticketTypeId;
    private UUID ticketStatusId;
    private List<UUID> observers;
    private List<UUID> executors;
    private List<UUID> files;
}
