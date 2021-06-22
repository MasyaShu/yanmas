package ru.itterminal.yanmas.aau.model.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.commons.model.dto.BaseEntityDto;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
@EqualsAndHashCode(callSuper = true)
public class PropertyValuesDtoResponse extends BaseEntityDto {
    private BaseEntityDto entity;

    private BaseEntityDto property;

    private String value;
}
