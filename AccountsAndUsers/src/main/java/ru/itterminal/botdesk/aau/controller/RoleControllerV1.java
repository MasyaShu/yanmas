package ru.itterminal.botdesk.aau.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.dto.RoleDto;
import ru.itterminal.botdesk.aau.service.impl.RoleServiceImpl;
import ru.itterminal.botdesk.commons.controller.BaseController;

@Slf4j
@RestController("RoleControllerV1")
@Validated
@RequestMapping("api/v1/role")
public class RoleControllerV1 extends BaseController {

    final RoleServiceImpl service;

    @Autowired
    public RoleControllerV1(RoleServiceImpl service) {
        this.service = service;
    }

    private final String ENTITY_NAME = Role.class.getSimpleName();


    @GetMapping()
    public ResponseEntity<List<RoleDto>> getAll() {
        log.debug(FIND_ALL_INIT_MESSAGE, ENTITY_NAME);
        List<Role> foundRoles = service.findAll();
        List<RoleDto> returnedRole = mapList(foundRoles, RoleDto.class);
        log.debug(FIND_ALL_FINISH_MESSAGE, ENTITY_NAME);
        return new ResponseEntity<>(returnedRole, HttpStatus.OK);
    }

}
