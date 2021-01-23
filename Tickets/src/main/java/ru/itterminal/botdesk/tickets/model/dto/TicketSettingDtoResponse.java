package ru.itterminal.botdesk.tickets.model.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;

import java.util.List;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class TicketSettingDtoResponse extends BaseEntityDto {

    private BaseEntityDto author;

    private BaseEntityDto group;

    List<BaseEntityDto> observers;

    List<BaseEntityDto> executors;

    private BaseEntityDto ticketTypeForNew;

    private BaseEntityDto ticketStatusForNew;

    private BaseEntityDto ticketStatusForReopen;

    private BaseEntityDto ticketStatusForClose;

    private BaseEntityDto ticketStatusForCancel;

}
