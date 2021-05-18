package ru.itterminal.yanmas.aau.controller;

import io.jsonwebtoken.JwtBuilder;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.itterminal.yanmas.aau.model.WhoWatchedEntity;
import ru.itterminal.yanmas.aau.model.dto.WhoWatchedEntityDtoRequest;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.WhoWatchedEntityServiceImpl;
import ru.itterminal.yanmas.commons.controller.BaseController;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.commons.model.validator.scenario.Delete;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;

@RestController
@Validated
@RequestMapping("api/v1/watched-entities")
@AllArgsConstructor
public class WhoWatchedEntityControllerV1 extends BaseController {

    private final WhoWatchedEntityServiceImpl service;
    private final UserServiceImpl userService;

    private final String ENTITY_NAME = WhoWatchedEntity.class.getSimpleName(); //NOSONAR

    @PostMapping()
    public ResponseEntity<String> watched(@Validated(Create.class) @RequestBody WhoWatchedEntityDtoRequest request) {
        service.watched(request.getEntitiesId(), userService.getCurrentUserFromJwtUser());
        return ResponseEntity.ok("Done");
    }

    @DeleteMapping()
    public ResponseEntity<String> unwatched(@Validated(Delete.class) @RequestBody WhoWatchedEntityDtoRequest request) {
        service.unwatched(request.getEntitiesId(), userService.getCurrentUserFromJwtUser());
        return ResponseEntity.ok("Done");
    }


}
