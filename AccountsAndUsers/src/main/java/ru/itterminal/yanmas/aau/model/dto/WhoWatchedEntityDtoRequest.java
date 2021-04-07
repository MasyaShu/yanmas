package ru.itterminal.yanmas.aau.model.dto;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.commons.model.validator.scenario.Delete;

import javax.validation.constraints.NotEmpty;
import java.util.List;
import java.util.UUID;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class WhoWatchedEntityDtoRequest {
    @NotEmpty(groups = {Create.class, Delete.class})
    private List<UUID> entitiesId;
}
