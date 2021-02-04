package ru.itterminal.botdesk.files.model.dto;

import java.util.UUID;

import javax.validation.constraints.NotNull;
import javax.validation.constraints.Null;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
@EqualsAndHashCode(callSuper = true)
public class FileDto extends BaseEntityDto {

    @NotNull(groups = {Create.class})
    @Null(groups = {Update.class})
    private String fileName;

    @Null(groups = {Create.class, Update.class})
    private Integer size;

    @Null(groups = {Create.class, Update.class})
    private Long createdAt;

    private UUID entityId;

    private Boolean isUploaded;
}
