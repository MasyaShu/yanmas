package ru.itterminal.botdesk.aau.controller;

import java.util.List;
import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.HttpRequestMethodNotSupportedException;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
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
import ru.itterminal.botdesk.commons.model.validator.scenario.Delete;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;

@Slf4j
@RestController("RoleControllerV1")
@Validated
@RequestMapping("v1/role")
public class RoleController extends BaseController {

    // TODO remove all except getAll

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
        RoleDto returnedRole = modelMapper.map(createdRole, RoleDto.class);
        log.info(CREATE_FINISH_MESSAGE, ENTITY_NAME, createdRole);
        return new ResponseEntity<>(returnedRole, HttpStatus.CREATED);
    }

    /**
     * Update a role
     *
     * @param requestDto contains all parameters of update role
     * @return updated role
     */
    @PutMapping()
    public ResponseEntity<RoleDto> update(@Validated(Update.class) @RequestBody RoleDto requestDto) {
        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, requestDto);
        Role role = modelMapper.map(requestDto, Role.class);
        Role updatedRole = service.update(role);
        RoleDto returnedRole = modelMapper.map(updatedRole, RoleDto.class);
        log.info(UPDATE_FINISH_MESSAGE, ENTITY_NAME, updatedRole);
        return new ResponseEntity<>(returnedRole, HttpStatus.OK);
    }

    /**
     * Get a role by ID
     *
     * @param id for find role in database
     * @return role
     */
    @GetMapping("/{id}")
    public ResponseEntity<RoleDto> getById(@PathVariable UUID id) {
        log.debug(FIND_BY_ID_INIT_MESSAGE, ENTITY_NAME, id);
        Role foundRole = service.findById(id);
        RoleDto returnedRole = modelMapper.map(foundRole, RoleDto.class);
        log.debug(FIND_BY_ID_FINISH_MESSAGE, ENTITY_NAME, foundRole);
        return new ResponseEntity<>(returnedRole, HttpStatus.OK);
    }

    /**
     * Get list of roles
     *
     * @return list of roles
     */
    @GetMapping()
    public ResponseEntity<List<RoleDto>> getAll() {
        log.debug(FIND_ALL_INIT_MESSAGE, ENTITY_NAME);
        List<Role> foundRoles = service.findAll();
        List<RoleDto> returnedRole = mapList(foundRoles, RoleDto.class);
        log.debug(FIND_ALL_FINISH_MESSAGE, ENTITY_NAME);
        return new ResponseEntity<>(returnedRole, HttpStatus.OK);
    }

    /**
     * Physical delete a role in database
     *
     * @param request RoleDto
     */
    @DeleteMapping()
    ResponseEntity<Void> physicalDelete(@Validated(Delete.class) @RequestBody RoleDto request)
            throws HttpRequestMethodNotSupportedException {
        // TODO Physical delete a bank in database
        throw new HttpRequestMethodNotSupportedException("Physical delete will be implement in the further");
    }

}
