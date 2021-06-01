package ru.itterminal.yanmas.files.model.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.commons.model.dto.BaseEntityDto;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.commons.model.validator.scenario.Update;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Null;
import java.util.UUID;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
@EqualsAndHashCode(callSuper = true)
public class FileDto extends BaseEntityDto {

    @NotBlank(groups = {Create.class})
    @Null(groups = {Update.class})
    private String fileName;

    @Null(groups = {Create.class, Update.class})
    private Integer size;

    @Null(groups = {Create.class, Update.class})
    private Long createdAt;

    private UUID entityId;

    private Boolean isUploaded;
}
