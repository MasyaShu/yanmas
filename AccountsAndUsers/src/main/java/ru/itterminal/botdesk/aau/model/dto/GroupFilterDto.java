package ru.itterminal.botdesk.aau.model.dto;

import lombok.*;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;
import ru.itterminal.botdesk.commons.model.validator.ValueOfEnum;

import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;
import java.util.List;
import java.util.UUID;

import static ru.itterminal.botdesk.aau.util.AAUConstants.INVALID_EMAIL;
import static ru.itterminal.botdesk.aau.util.AAUConstants.emailPattern;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class GroupFilterDto extends BaseFilterDto {

    @Size(min = 1, max = 128)
    private String name;

    private String comment;

    private Boolean isDeprecated;

    private Boolean isInner;

    @ValueOfEnum(enumClass = FieldsForSort.class, message = "must be any of: name")
    private String sortBy = "name";

    public enum FieldsForSort {
        NAME
    }

}
