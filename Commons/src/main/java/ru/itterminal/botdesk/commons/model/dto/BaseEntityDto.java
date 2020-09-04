package ru.itterminal.botdesk.commons.model.dto;

import static ru.itterminal.botdesk.commons.util.CommonConstants.MUST_BE_NULL_FOR_THE_NEW_ENTITY;

import java.util.UUID;

import javax.validation.constraints.AssertFalse;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Null;

import lombok.Getter;
import lombok.Setter;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Delete;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;
import ru.itterminal.botdesk.commons.util.CommonConstants;

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

    @Null(groups = Create.class,
        message = MUST_BE_NULL_FOR_THE_NEW_ENTITY)
    @NotNull(groups = Update.class)
    @Max(value = 32766, groups = Update.class)
    @Min(value = 0, groups = Update.class)
    private Integer version;


    @NotNull(groups = {Create.class, Update.class, Delete.class})
    @AssertFalse(groups = {Create.class},
        message = CommonConstants.DELETED_ASSERT_FALSE)
    private Boolean deleted;

}

