package ru.itterminal.botdesk.aau.model.dto;

import static ru.itterminal.botdesk.aau.util.AAUConstants.INVALID_EMAIL;
import static ru.itterminal.botdesk.aau.util.AAUConstants.EMAIL_PATTERN;

import java.util.List;
import java.util.UUID;

import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;
import ru.itterminal.botdesk.commons.model.validator.ValueOfEnum;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class UserFilterDto extends BaseFilterDto {

    @Pattern(regexp = EMAIL_PATTERN,
            message = INVALID_EMAIL)
    private String email;

    @Size(min = 1, max = 20)
    private String firstName;

    @Size(min = 1, max = 30)
    private String secondName;

    @Size(min = 6, max = 30)
    private String phone;

    private String comment;

    private Boolean isArchived;

    private List<UUID> groups;

    private List<UUID> roles;

    @ValueOfEnum(enumClass = FieldsForSort.class, message = "must be any of: firstName, secondName")
    private String sortBy = "firstName";

    public enum FieldsForSort {
        FIRSTNAME, SECONDNAME
    }

}
