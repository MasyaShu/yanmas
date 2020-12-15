package ru.itterminal.botdesk.commons.model.dto;

import static ru.itterminal.botdesk.commons.util.CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY;

import java.util.UUID;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Null;
import javax.validation.constraints.Size;

import lombok.Getter;
import lombok.Setter;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Delete;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;

/**
 * BaseEntityDto
 *
 */
@Getter
@Setter
public class BaseEntityDto {

    @Null(groups = Create.class,
        message = MUST_BE_NULL_FOR_THE_NEW_ENTITY)
    @NotNull(groups = {Update.class, Delete.class})
    private UUID id;

    @Size(max = 128, groups = {Update.class, Create.class})
    private String outId;

    @Null(groups = Create.class,
        message = MUST_BE_NULL_FOR_THE_NEW_ENTITY)
    @NotNull(groups = Update.class)
    @Max(value = 2147483647, groups = Update.class)
    @Min(value = 0, groups = Update.class)
    private Integer version;


    @Null(groups = Create.class, message = MUST_BE_NULL_FOR_THE_NEW_ENTITY)
    @NotNull(groups = {Update.class, Delete.class})
    private Boolean deleted;

}

