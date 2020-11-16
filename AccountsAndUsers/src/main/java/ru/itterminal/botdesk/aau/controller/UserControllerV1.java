package ru.itterminal.botdesk.aau.controller;

import static java.lang.String.format;

import java.security.Principal;
import java.util.List;
import java.util.UUID;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.dto.UserDto;
import ru.itterminal.botdesk.aau.model.dto.UserDtoResponseWithoutPassword;
import ru.itterminal.botdesk.aau.model.dto.UserFilterDto;
import ru.itterminal.botdesk.aau.model.spec.UserSpec;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.GroupServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.RoleServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;
import ru.itterminal.botdesk.security.jwt.JwtUser;

@SuppressWarnings("DuplicatedCode")
@Slf4j
@RestController("UserControllerV1")
@Validated
@RequestMapping("api/v1/user")
public class UserControllerV1 extends BaseController {

    final UserServiceImpl userService;
    final AccountServiceImpl accountService;
    final RoleServiceImpl roleService;
    final GroupServiceImpl groupService;
    final UserSpec spec;

    @Autowired
    public UserControllerV1(UserServiceImpl service, UserSpec userSpec,
            AccountServiceImpl accountService, RoleServiceImpl roleService,
            GroupServiceImpl groupService) {
        this.spec = userSpec;
        this.userService = service;
        this.accountService = accountService;
        this.roleService = roleService;
        this.groupService = groupService;
    }

    private final String ENTITY_NAME = User.class.getSimpleName();

    @PostMapping()
    @ResponseStatus(value = HttpStatus.CREATED)
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR')")
    public ResponseEntity<UserDtoResponseWithoutPassword> create(Principal principal,
            @Validated(Create.class) @RequestBody UserDto request) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, request);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        User user = modelMapper.map(request, User.class);
        user.setAccount(accountService.findById(jwtUser.getAccountId()));
        user.setRole(roleService.findById(request.getRoleId()));
        user.setOwnGroup(groupService.findById(request.getGroupId()));
        User createdUser = userService.create(user);
        UserDtoResponseWithoutPassword returnedUser =
                modelMapper.map(createdUser, UserDtoResponseWithoutPassword.class);
        log.info(CREATE_FINISH_MESSAGE, ENTITY_NAME, createdUser);
        return new ResponseEntity<>(returnedUser, HttpStatus.CREATED);
    }

    @PostMapping("/check-access")
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR')")
    public ResponseEntity<String> createCheckAccess() {
        String message = format(SUCCESSFUL_CHECK_ACCESS, WORD_CREATE, ENTITY_NAME);
        log.trace(message);
        return ResponseEntity.ok(message);
    }

    @PutMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR')")
    public ResponseEntity<UserDtoResponseWithoutPassword> update( Principal principal,
            @Validated(Update.class) @RequestBody UserDto request) {
        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, request);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        User user = modelMapper.map(request, User.class);
        user.setAccount(accountService.findById(jwtUser.getAccountId()));
        user.setRole(roleService.findById(request.getRoleId()));
        user.setOwnGroup(groupService.findById(request.getGroupId()));
        User updatedUser = userService.update(user);
        UserDtoResponseWithoutPassword returnedUser =
                modelMapper.map(updatedUser, UserDtoResponseWithoutPassword.class);
        log.info(UPDATE_FINISH_MESSAGE, ENTITY_NAME, updatedUser);
        return new ResponseEntity<>(returnedUser, HttpStatus.OK);
    }

    @PutMapping("/check-access")
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR')")
    public ResponseEntity<String> updateCheckAccess() {
        String message = format(SUCCESSFUL_CHECK_ACCESS, WORD_UPDATE, ENTITY_NAME);
        log.trace(message);
        return ResponseEntity.ok(message);
    }

    @GetMapping("/{id}")
    public ResponseEntity<UserDtoResponseWithoutPassword> getById(Principal user, @PathVariable UUID id) {
        log.debug(FIND_BY_ID_INIT_MESSAGE, ENTITY_NAME, id);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
        User foundUser;
        if (jwtUser.isInnerGroup()) {
            foundUser = userService.findByIdAndAccountId(id, jwtUser.getAccountId());
        } else {
            foundUser = userService.findByIdAndAccountIdAndOwnGroupId(id, jwtUser.getAccountId(), jwtUser.getGroupId());
        }
        UserDtoResponseWithoutPassword returnedUser = modelMapper.map(foundUser, UserDtoResponseWithoutPassword.class);
        log.debug(FIND_BY_ID_FINISH_MESSAGE, ENTITY_NAME, foundUser);
        return new ResponseEntity<>(returnedUser, HttpStatus.OK);
    }

    @SuppressWarnings("ConstantConditions")
    @GetMapping()
    public ResponseEntity<Page<UserDtoResponseWithoutPassword>> getByFilter(
            Principal user,
            @Valid @RequestBody UserFilterDto filter,
            @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
            @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        log.debug(FIND_INIT_MESSAGE, ENTITY_NAME, page, size, filter);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
        if (filter.getDirection() == null) {
            filter.setDirection("ASC");
        }
        if (filter.getSortBy() == null) {
            filter.setSortBy("firstName");
        }
        if (filter.getDeleted() == null) {
            filter.setDeleted("all");
        }
        Pageable pageable =
                PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(filter.getDirection()),
                        filter.getSortBy()));
        Page<User> foundUsers;
        Page<UserDtoResponseWithoutPassword> returnedUsers;
        Specification<User> userSpecification = Specification
                .where(filter.getEmail() == null ? null : spec.getUserByEmailSpec(filter.getEmail()))
                .and(filter.getFirstName() == null ? null : spec.getUserByFirstNameSpec(filter.getFirstName()))
                .and(filter.getSecondName() == null ? null : spec.getUserBySecondNameSpec(filter.getSecondName()))
                .and(filter.getPhone() == null ? null : spec.getUserByPhoneSpec(filter.getPhone()))
                .and(filter.getComment() == null ? null : spec.getUserByCommentSpec(filter.getComment()))
                .and(filter.getIsArchived() == null ? null : spec.getUserByIsArchivedSpec(filter.getIsArchived()))
                .and(filter.getGroups() == null || filter.getGroups().isEmpty() ? null :
                        spec.getUserByListOfGroupsSpec(filter.getGroups()))
                .and(jwtUser.isInnerGroup() ? null :
                        spec.getUserByListOfGroupsSpec(List.of(jwtUser.getGroupId())))
                .and(filter.getRoles() == null || filter.getRoles().isEmpty() ? null :
                        spec.getUserByListOfRolesSpec(filter.getRoles()))
                .and(spec.getEntityByDeletedSpec(BaseFilterDto.FilterByDeleted.fromString(filter.getDeleted())))
                .and(spec.getEntityByAccountSpec(jwtUser.getAccountId()))
                .and(filter.getOutId() == null ? null :  spec.getEntityByOutIdSpec(filter.getOutId()));
        foundUsers = userService.findAllByFilter(userSpecification, pageable);
        returnedUsers = mapPage(foundUsers, UserDtoResponseWithoutPassword.class, pageable);
        log.debug(FIND_FINISH_MESSAGE, ENTITY_NAME, foundUsers.getTotalElements());
        return new ResponseEntity<>(returnedUsers, HttpStatus.OK);
    }

    @DeleteMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<Void> physicalDelete() {
        throw new UnsupportedOperationException("Physical delete will be implement in the further");
    }
}
