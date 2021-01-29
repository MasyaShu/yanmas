package ru.itterminal.botdesk.tickets.model.dto;

import java.util.List;
import java.util.UUID;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
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
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class TicketDtoRequest extends BaseEntityDto {
    @NotNull(groups = {Create.class, Update.class})
    private UUID author;

    @Size(max = 256, groups = {Create.class, Update.class})
    private String subject;

    private String description;
    private Long deadline;
    private Boolean isFinished;
    private UUID ticketType;
    private UUID ticketStatus;
    private UUID ticketTemplate;
    private List<UUID> observers;
    private List<UUID> executors;
    private List<UUID> files;
}