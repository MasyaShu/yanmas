package ru.itterminal.yanmas.aau.controller;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.model.dto.UserDtoRequest;
import ru.itterminal.yanmas.aau.model.dto.UserDtoResponse;
import ru.itterminal.yanmas.aau.model.dto.UserFilterDto;
import ru.itterminal.yanmas.aau.service.impl.AccountServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.GroupServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.RoleServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.commons.controller.BaseController;
import ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.commons.model.validator.scenario.Update;
import ru.itterminal.yanmas.security.jwt.JwtUser;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;
import java.security.Principal;
import java.util.List;
import java.util.UUID;

import static ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.EXIST_IN;

@SuppressWarnings("DuplicatedCode")
@Slf4j
@RestController("UserControllerV1")
@Validated
@RequestMapping("api/v1/user")
@AllArgsConstructor
public class UserControllerV1 extends BaseController {

    private final UserServiceImpl userService;
    private final AccountServiceImpl accountService;
    private final RoleServiceImpl roleService;
    private final GroupServiceImpl groupService;
    private final SpecificationsFactory specFactory;

    private final String ENTITY_NAME = User.class.getSimpleName();

    @PostMapping()
    @ResponseStatus(value = HttpStatus.CREATED)
    public ResponseEntity<UserDtoResponse> create(Principal principal,
                                                  @Validated(Create.class) @RequestBody UserDtoRequest request) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, request);
        var user = modelMapper.map(request, User.class);
        var jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        // !!! setting is here because it is not possible into service layer (cycle dependency with AccountServiceImpl)
        user.setAccount(accountService.findById(jwtUser.getAccountId()));
        user.setRole(roleService.findById(request.getRole()));
        user.setGroup(groupService.findByIdAndAccountId(request.getGroup()));
        var createdUser = userService.create(user);
        var returnedUser = modelMapper.map(createdUser, UserDtoResponse.class);
        log.info(CREATE_FINISH_MESSAGE, ENTITY_NAME, createdUser);
        return new ResponseEntity<>(returnedUser, HttpStatus.CREATED);
    }

    @PutMapping()
    public ResponseEntity<UserDtoResponse> update(Principal principal,
                                                  @Validated(Update.class) @RequestBody UserDtoRequest request) {
        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, request);
        var user = modelMapper.map(request, User.class);
        var jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        user.setAccount(accountService.findById(jwtUser.getAccountId()));
        user.setRole(roleService.findById(request.getRole()));
        user.setGroup(groupService.findByIdAndAccountId(request.getGroup()));
        var updatedUser = userService.update(user);
        var returnedUser = modelMapper.map(updatedUser, UserDtoResponse.class);
        log.info(UPDATE_FINISH_MESSAGE, ENTITY_NAME, updatedUser);
        return new ResponseEntity<>(returnedUser, HttpStatus.OK);
    }

    @GetMapping("/{id}")
    public ResponseEntity<UserDtoResponse> getById(@PathVariable UUID id) {
        log.debug(FIND_BY_ID_INIT_MESSAGE, ENTITY_NAME, id);
        var foundUser = userService.findByIdAndAccountId(id);
        var returnedUser = modelMapper.map(foundUser, UserDtoResponse.class);
        log.debug(FIND_BY_ID_FINISH_MESSAGE, ENTITY_NAME, foundUser);
        return new ResponseEntity<>(returnedUser, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<Page<UserDtoResponse>> getByFilter(
            Principal user,
            @Valid @RequestBody UserFilterDto filterDto,
            @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
            @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        log.debug(FIND_INIT_MESSAGE, ENTITY_NAME, page, size, filterDto);
        var jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
        var accountId = jwtUser.getAccountId();
        if (jwtUser.getWeightRole() <= Roles.AUTHOR.getWeight() || !jwtUser.isInnerGroup()) {
            var groupFilter = BaseEntityFilter.builder()
                    .typeComparison(EXIST_IN.toString())
                    .listOfIdEntities(List.of(jwtUser.getGroupId()))
                    .build();
            filterDto.setGroup(groupFilter);
        }
        var pageable = createPageable(size, page, filterDto.getSortByFields(), filterDto.getSortDirection());
        var userSpecification = specFactory.makeSpecificationFromEntityFilterDto(User.class, filterDto, accountId);
        var foundUsers = userService.findAllByFilter(userSpecification, pageable);
        var returnedUsers = mapPage(foundUsers, UserDtoResponse.class, pageable);
        log.debug(FIND_FINISH_MESSAGE, ENTITY_NAME, foundUsers.getTotalElements());
        return new ResponseEntity<>(returnedUsers, HttpStatus.OK);
    }
}
