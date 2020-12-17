package ru.itterminal.botdesk.tickets.model.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.aau.model.dto.UserFilterDto;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;
import ru.itterminal.botdesk.commons.model.validator.ValueOfEnum;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@ToString
public class TicketSettingFilterDto extends BaseFilterDto {

//    @ValueOfEnum(enumClass = UserFilterDto.FieldsForSort.class, message = "must be any of: firstName, secondName")
//    private String sortBy = "firstName";
//
//    @SuppressWarnings("unused")
//    public enum FieldsForSort {
//        FIRSTNAME, SECONDNAME
//    }
}
