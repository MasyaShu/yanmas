package ru.itterminal.botdesk.tickets.model.dto;

import java.util.List;

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
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class TicketDtoResponse extends BaseEntityDto {
    private Long number;
    private Long createdAt;
    private BaseEntityDto author;
    private String subject;
    private String description;
    private Long deadline;
    private Boolean isFinished;
    private BaseEntityDto ticketType;
    private BaseEntityDto ticketStatus;
    private BaseEntityDto ticketTemplate;
    private List<BaseEntityDto> observers;
    private List<BaseEntityDto> executors;
    private List<BaseEntityDto> files;
}
