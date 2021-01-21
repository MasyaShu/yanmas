package ru.itterminal.botdesk.aau.model.dto;

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
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;
import ru.itterminal.botdesk.commons.model.validator.ValueOfEnum;
import ru.itterminal.botdesk.aau.util.AAUConstants;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class UserFilterDto extends BaseFilterDto {

    @Pattern(regexp = AAUConstants.EMAIL_PATTERN,
            message = AAUConstants.INVALID_EMAIL)
    private String email;

    @Size(max = 128)
    private String name;

    @Size(min = 6, max = 30)
    private String phone;

    private String comment;

    private Boolean isArchived;

    private List<UUID> groups;

    private List<UUID> roles;

}
