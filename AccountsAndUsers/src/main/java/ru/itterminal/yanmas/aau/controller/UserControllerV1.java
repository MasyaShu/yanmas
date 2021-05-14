package ru.itterminal.yanmas.aau.controller;

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
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.model.dto.UserDtoRequest;
import ru.itterminal.yanmas.aau.model.dto.UserDtoResponse;
import ru.itterminal.yanmas.aau.model.dto.UserFilterDto;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.commons.model.validator.scenario.Update;

@SuppressWarnings({"unchecked", "rawtypes"})
@RestController("UserControllerV1")
@Validated
@RequestMapping("api/v1/user")
@AllArgsConstructor
public class UserControllerV1 extends BaseControllerImpl<
        User,
        UserServiceImpl,
        UserDtoRequest,
        UserDtoResponse,
        UserFilterDto>  {

    private static final Class responseClazz = UserDtoResponse.class;
    private static final Class entityClazz = User.class;


    @PostMapping()
    @ResponseStatus(value = HttpStatus.CREATED)
    public ResponseEntity<UserDtoResponse> create(@Validated(Create.class) @RequestBody UserDtoRequest request) {
        return create(request, entityClazz, responseClazz);
    }

    @PutMapping()
    public ResponseEntity<UserDtoResponse> update(@Validated(Update.class) @RequestBody UserDtoRequest request) {
        return update(request, entityClazz, responseClazz);
    }

    @GetMapping("/{id}")
    public ResponseEntity<UserDtoResponse> getById(@PathVariable UUID id) {
        return getById(id, responseClazz);
    }

    @GetMapping()
    public ResponseEntity<Page<UserDtoResponse>> getByFilter(
            @Valid @RequestBody UserFilterDto filter,
            @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
            @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        return getByFilter(filter, page, size, entityClazz, responseClazz);
    }

}
