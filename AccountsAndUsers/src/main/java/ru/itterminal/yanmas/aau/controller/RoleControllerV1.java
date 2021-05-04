package ru.itterminal.yanmas.aau.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import ru.itterminal.yanmas.aau.model.Role;
import ru.itterminal.yanmas.aau.model.dto.RoleDto;
import ru.itterminal.yanmas.aau.service.impl.RoleServiceImpl;
import ru.itterminal.yanmas.commons.controller.BaseController;

import java.util.List;

@Slf4j
@RestController("RoleControllerV1")
@Validated
@RequestMapping("api/v1/user/role")
@RequiredArgsConstructor
public class RoleControllerV1 extends BaseController {

    final RoleServiceImpl service;

    private final String entityName = Role.class.getSimpleName();

    @GetMapping()
    public ResponseEntity<List<RoleDto>> getAll() {
        log.debug(FIND_ALL_INIT_MESSAGE, entityName);
        List<Role> foundRoles = service.findAll();
        List<RoleDto> returnedRole = mapList(foundRoles, RoleDto.class);
        log.debug(FIND_ALL_FINISH_MESSAGE, entityName);
        return new ResponseEntity<>(returnedRole, HttpStatus.OK);
    }

}
