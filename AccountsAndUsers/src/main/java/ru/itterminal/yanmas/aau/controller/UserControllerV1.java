package ru.itterminal.yanmas.aau.controller;

import static ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.EXIST_IN;

import java.util.List;
import java.util.UUID;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;

import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
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
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;

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
    private final  JwtUserBuilder jwtUserBuilder;

    @SuppressWarnings("DuplicatedCode")
    @PostMapping()
    @ResponseStatus(value = HttpStatus.CREATED)
    public ResponseEntity<UserDtoResponse> create(@Validated(Create.class) @RequestBody UserDtoRequest request) {
        var user = modelMapper.map(request, User.class);
        var currentUser = getCurrentUser();
        // !!! setting is here because it is not possible into service layer (cycle dependency with AccountServiceImpl)
        user.setAccount(accountService.findById(currentUser.getAccount().getId()));
        user.setRole(roleService.findById(request.getRole()));
        user.setGroup(groupService.findByIdAndAccountId(request.getGroup(), currentUser));
        var createdUser = userService.create(user, currentUser);
        var returnedUser = modelMapper.map(createdUser, UserDtoResponse.class);
        return new ResponseEntity<>(returnedUser, HttpStatus.CREATED);
    }

    @SuppressWarnings("DuplicatedCode")
    @PutMapping()
    public ResponseEntity<UserDtoResponse> update(@Validated(Update.class) @RequestBody UserDtoRequest request) {
        var user = modelMapper.map(request, User.class);
        var currentUser = getCurrentUser();
        user.setAccount(accountService.findById(currentUser.getAccount().getId()));
        user.setRole(roleService.findById(request.getRole()));
        user.setGroup(groupService.findByIdAndAccountId(request.getGroup(), currentUser));
        var updatedUser = userService.update(user, currentUser);
        var returnedUser = modelMapper.map(updatedUser, UserDtoResponse.class);
        return new ResponseEntity<>(returnedUser, HttpStatus.OK);
    }

    @GetMapping("/{id}")
    public ResponseEntity<UserDtoResponse> getById(@PathVariable UUID id) {
        var currentUser = getCurrentUser();
        var foundUser = userService.findByIdAndAccountId(id, currentUser);
        var returnedUser = modelMapper.map(foundUser, UserDtoResponse.class);
        return new ResponseEntity<>(returnedUser, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<Page<UserDtoResponse>> getByFilter(
            @Valid @RequestBody UserFilterDto filterDto,
            @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
            @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        var jwtUser = jwtUserBuilder.getJwtUser();
        var currentUser = userService.findById(jwtUser.getId());
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
        var foundUsers = userService.findAllByFilter(userSpecification, pageable, currentUser);
        var returnedUsers = mapPage(foundUsers, UserDtoResponse.class, pageable);
        return new ResponseEntity<>(returnedUsers, HttpStatus.OK);
    }

    private User getCurrentUser() {
        var jwtUser = jwtUserBuilder.getJwtUser();
        return userService.findById(jwtUser.getId());
    }
}
