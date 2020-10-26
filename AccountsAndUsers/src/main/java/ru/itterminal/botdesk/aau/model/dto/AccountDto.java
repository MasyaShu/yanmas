package ru.itterminal.botdesk.aau.model.dto;

import javax.validation.constraints.Size;
import javax.validation.constraints.NotNull;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@ToString
public class AccountDto extends BaseEntityDto {
    @NotNull(groups = {Update.class})
    @Size(min = 3, max = 128, groups = {Update.class})
    private String name;
}
