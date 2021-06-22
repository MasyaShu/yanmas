package ru.itterminal.yanmas.aau.model.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.commons.model.dto.BaseEntityDto;

import java.util.UUID;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
@EqualsAndHashCode(callSuper = true)
public class PropertyValuesDtoRequest extends BaseEntityDto {
    private UUID entityId;

    private UUID propertyId;

    private String value;
}
