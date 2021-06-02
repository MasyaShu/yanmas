package ru.itterminal.yanmas.files.model.dto;

import java.util.UUID;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Null;
import javax.validation.constraints.Size;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.commons.model.dto.BaseEntityDto;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
@EqualsAndHashCode(callSuper = true)
public class FileDto extends BaseEntityDto {

    @NotBlank(groups = {Create.class})
    @Size(max = 256, groups = {Create.class})
    private String fileName;

    @Null(groups = {Create.class})
    private Integer size;

    @Null(groups = {Create.class})
    private Long createdAt;

    @Null(groups = {Create.class})
    private UUID entityId;

    @Null(groups = {Create.class})
    private Boolean isUploaded;
}
