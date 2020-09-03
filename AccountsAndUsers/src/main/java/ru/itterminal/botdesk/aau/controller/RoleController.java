package ru.itterminal.botdesk.aau.controller;

import javax.validation.Valid;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.dto.RoleDto;
import ru.itterminal.botdesk.aau.service.impl.RoleServiceImpl;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;

@Slf4j
@RestController("RoleControllerV1")
@Validated
@RequestMapping("v1/role")
public class RoleController extends BaseController {
    RoleServiceImpl service;

    @Autowired
    public RoleController(RoleServiceImpl service) {
        this.service = service;
    }

    private final String ENTITY_NAME = Role.class.getSimpleName();

    /**
     * Create a role
     *
     * @param request contains parameters for create new role.
     *                Not null fields: name
     * @return new created role
     */
    @PostMapping()
    @ResponseStatus(value = HttpStatus.CREATED)
    public ResponseEntity<RoleDto> create(@Validated(Create.class) @RequestBody RoleDto request) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, request);
        Role role = modelMapper.map(request, Role.class);
        Role createdRole = service.create(role);
        RoleDto returnedBank = modelMapper.map(createdRole, RoleDto.class);
        log.info(CREATE_FINISH_MESSAGE, ENTITY_NAME, createdRole);
        return new ResponseEntity<>(returnedBank, HttpStatus.CREATED);
    }
}
