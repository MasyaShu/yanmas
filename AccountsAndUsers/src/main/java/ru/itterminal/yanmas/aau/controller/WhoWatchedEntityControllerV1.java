package ru.itterminal.yanmas.aau.controller;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.itterminal.yanmas.aau.model.WhoWatchedEntity;
import ru.itterminal.yanmas.aau.model.dto.WhoWatchedEntityDtoRequest;
import ru.itterminal.yanmas.aau.service.impl.WhoWatchedEntityServiceImpl;
import ru.itterminal.yanmas.commons.controller.BaseController;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.commons.model.validator.scenario.Delete;

@Slf4j
@RestController
@Validated
@RequestMapping("api/v1/watched-entities")
@AllArgsConstructor
public class WhoWatchedEntityControllerV1 extends BaseController {

    private final WhoWatchedEntityServiceImpl service;

    private final String ENTITY_NAME = WhoWatchedEntity.class.getSimpleName(); //NOSONAR

    @PostMapping()
    public ResponseEntity<String> watched(@Validated(Create.class) @RequestBody WhoWatchedEntityDtoRequest request) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, request);
        service.watched(request.getEntitiesId());
        log.info(CREATE_FINISH_MESSAGE, ENTITY_NAME, request);
        return ResponseEntity.ok("Done");
    }

    @DeleteMapping()
    public ResponseEntity<String> unwatched(@Validated(Delete.class) @RequestBody WhoWatchedEntityDtoRequest request) {
        log.debug(DELETE_INIT_MESSAGE, ENTITY_NAME, request);
        service.unwatched(request.getEntitiesId());
        log.info(DELETE_FINISH_MESSAGE, ENTITY_NAME, request);
        return ResponseEntity.ok("Done");
    }


}
