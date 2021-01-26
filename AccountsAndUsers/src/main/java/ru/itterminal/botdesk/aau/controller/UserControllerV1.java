package ru.itterminal.botdesk.aau.controller;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.EXIST_IN;

import java.security.Principal;
import java.util.List;
import java.util.UUID;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;

import org.springframework.data.domain.Page;
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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.dto.UserDtoRequest;
import ru.itterminal.botdesk.aau.model.dto.UserDtoResponse;
import ru.itterminal.botdesk.aau.model.dto.UserFilterDto;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.GroupServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.RoleServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.model.filter.BaseEntityFilter;
import ru.itterminal.botdesk.commons.model.spec.SpecificationsFactory;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;
import ru.itterminal.botdesk.security.jwt.JwtUser;

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
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR')")
    public ResponseEntity<UserDtoResponse> create(Principal principal,
                                                  @Validated(Create.class) @RequestBody UserDtoRequest request) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, request);

        User user = modelMapper.map(request, User.class);

        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        user.setAccount(accountService.findById(jwtUser.getAccountId()));
        UUID accountId = user.getAccount().getId();

        user.setRole(roleService.findById(request.getRole()));
        user.setGroup(groupService.findByIdAndAccountId(request.getGroup(), accountId));

        User createdUser = userService.create(user);

        UserDtoResponse returnedUser = modelMapper.map(createdUser, UserDtoResponse.class);

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
    public ResponseEntity<UserDtoResponse> update(Principal principal,
                                                  @Validated(Update.class) @RequestBody UserDtoRequest request) {
        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, request);
        User user = modelMapper.map(request, User.class);

        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        user.setAccount(accountService.findById(jwtUser.getAccountId()));
        UUID accountId = user.getAccount().getId();

        user.setRole(roleService.findById(request.getRole()));
        user.setGroup(groupService.findByIdAndAccountId(request.getGroup(), accountId));

        User updatedUser = userService.update(user);
        UserDtoResponse returnedUser =
                modelMapper.map(updatedUser, UserDtoResponse.class);
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
    public ResponseEntity<UserDtoResponse> getById(Principal user, @PathVariable UUID id) {
        log.debug(FIND_BY_ID_INIT_MESSAGE, ENTITY_NAME, id);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
        User foundUser;
        if (jwtUser.isInnerGroup()) {
            foundUser = userService.findByIdAndAccountId(id, jwtUser.getAccountId());
        } else {
            foundUser = userService.findByIdAndAccountIdAndGroupId(id, jwtUser.getAccountId(), jwtUser.getGroupId());
        }
        UserDtoResponse returnedUser = modelMapper.map(foundUser, UserDtoResponse.class);
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
        if (!jwtUser.isInnerGroup()) {
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

    @DeleteMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<Void> physicalDelete() {
        throw new UnsupportedOperationException("Physical delete will be implement in the further");
    }
}
