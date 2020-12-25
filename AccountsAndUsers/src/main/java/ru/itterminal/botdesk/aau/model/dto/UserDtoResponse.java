package ru.itterminal.botdesk.aau.model.dto;

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
@ToString
@EqualsAndHashCode(callSuper = true)
public class UserDtoResponse extends BaseEntityDto {

    private String email;

    private String name;

    private String phone;

    private String comment;

    private Boolean isArchived;

    private BaseEntityDto group;

    private BaseEntityDto role;

}
