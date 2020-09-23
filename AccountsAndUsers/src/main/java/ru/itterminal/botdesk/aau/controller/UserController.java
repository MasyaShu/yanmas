package ru.itterminal.botdesk.aau.controller;

import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.dto.UserDto;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;

@Slf4j
@RestController("UserControllerV1")
@Validated
@RequestMapping("v1/user")
public class UserController extends BaseController {

    UserServiceImpl service;

    @Autowired
    public UserController(UserServiceImpl service) {
        this.service = service;
    }

    private final String ENTITY_NAME = User.class.getSimpleName();

    /**
     * Create a user
     *
     * @param request contains parameters for create new user.
     *                Not null fields: email, password, account, group, roles
     * @return new created user
     */
    @PostMapping()
    @ResponseStatus(value = HttpStatus.CREATED)
    public ResponseEntity<UserDto> create(@Validated(Create.class) @RequestBody UserDto request) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, request);
        User user = modelMapper.map(request, User.class);
        User createdUser = service.create(user);
        UserDto returnedUser = modelMapper.map(createdUser, UserDto.class);
        returnedUser.setPassword(null);
        log.info(CREATE_FINISH_MESSAGE, ENTITY_NAME, createdUser);
        return new ResponseEntity<>(returnedUser, HttpStatus.CREATED);
    }

    /**
     * Update a user
     *
     * @param requestDto contains all parameters of update user
     * @return updated user
     */
    @PutMapping()
    public ResponseEntity<UserDto> update(@Validated(Update.class) @RequestBody UserDto requestDto) {
        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, requestDto);
        User user = modelMapper.map(requestDto, User.class);
        User updatedUser = service.update(user);
        UserDto returnedUser = modelMapper.map(updatedUser, UserDto.class);
        returnedUser.setPassword(null);
        log.info(UPDATE_FINISH_MESSAGE, ENTITY_NAME, updatedUser);
        return new ResponseEntity<>(returnedUser, HttpStatus.OK);
    }

    /**
     * Get a user by ID
     *
     * @param id for find user in database
     * @return user
     */
    @GetMapping("/{id}")
    public ResponseEntity<UserDto> getById(@PathVariable UUID id) {
        log.debug(FIND_BY_ID_INIT_MESSAGE, ENTITY_NAME, id);
        User foundUser = service.findById(id);
        UserDto returnedUser = modelMapper.map(foundUser, UserDto.class);
        log.debug(FIND_BY_ID_FINISH_MESSAGE, ENTITY_NAME, foundUser);
        return new ResponseEntity<>(returnedUser, HttpStatus.OK);
    }

//    /**
//     * Get list of users
//     *
//     * @return list of users
//     */
//    @GetMapping()
//    public ResponseEntity<List<UserDto>> getAll() {
//        log.debug(FIND_ALL_INIT_MESSAGE, ENTITY_NAME);
//        List<User> foundUsers = service.findAll();
//        List<UserDto> returnedUser = mapList(foundUsers, UserDto.class);
//        log.debug(FIND_ALL_FINISH_MESSAGE, ENTITY_NAME);
//        return new ResponseEntity<>(returnedUser, HttpStatus.OK);
//    }
//
//    /**
//     * Physical delete a user in database
//     *
//     * @param request UserDto
//     */
//    @DeleteMapping()
//    ResponseEntity<Void> physicalDelete(@Validated(Delete.class) @RequestBody UserDto request)
//            throws HttpRequestMethodNotSupportedException {
//        // TODO Physical delete a user in database
//        throw new HttpRequestMethodNotSupportedException("Physical delete will be implement in the further");
//    }
}
