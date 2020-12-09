package ru.itterminal.botdesk.files.model.dto;

import java.util.UUID;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class FileDto extends BaseEntityDto {

    private UUID entityId;

    private String fileName;

    private Integer size;

    private Long createdAt;
}
