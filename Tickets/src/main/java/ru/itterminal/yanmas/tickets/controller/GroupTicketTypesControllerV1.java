package ru.itterminal.yanmas.tickets.controller;

import java.security.Principal;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.UUID;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;

import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.yanmas.aau.util.ReflectionHelper;
import ru.itterminal.yanmas.commons.controller.BaseController;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.commons.model.validator.scenario.Update;
import ru.itterminal.yanmas.security.jwt.JwtUser;
import ru.itterminal.yanmas.tickets.model.GroupTicketTypes;
import ru.itterminal.yanmas.tickets.model.dto.GroupTicketTypesDtoRequest;
import ru.itterminal.yanmas.tickets.model.dto.GroupTicketTypesDtoResponse;
import ru.itterminal.yanmas.tickets.model.dto.GroupTicketTypesFilterDto;
import ru.itterminal.yanmas.tickets.service.impl.GroupTicketTypesServiceImpl;

@Slf4j
@RestController("GroupTicketTypesControllerV1")
@Validated
@RequestMapping("api/v1/group-ticket-types")
@RequiredArgsConstructor
public class GroupTicketTypesControllerV1 extends BaseController {

    private final GroupTicketTypesServiceImpl service;
    private final SpecificationsFactory specFactory;
    private final ReflectionHelper reflectionHelper;

    private final String ENTITY_NAME = GroupTicketTypes.class.getSimpleName(); //NOSONAR

    @PostMapping()
    public ResponseEntity<GroupTicketTypesDtoResponse> create(@Validated(Create.class) @RequestBody GroupTicketTypesDtoRequest request) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, request);
        request.setTicketTypes(new ArrayList<>(new HashSet<>(request.getTicketTypes())));
        var createdGroupTicketTypes = service.create(
                (GroupTicketTypes) reflectionHelper.convertRequestDtoIntoEntityWhereNestedObjectsWithOnlyValidId(
                        request,
                        GroupTicketTypes.class
                )
        );
        var returnedGroupTicketTypes = modelMapper.map(createdGroupTicketTypes, GroupTicketTypesDtoResponse.class);
        log.info(CREATE_FINISH_MESSAGE, ENTITY_NAME, createdGroupTicketTypes);
        return new ResponseEntity<>(returnedGroupTicketTypes, HttpStatus.CREATED);
    }

    @PutMapping()
    public ResponseEntity<GroupTicketTypesDtoResponse> update(@Validated(Update.class) @RequestBody GroupTicketTypesDtoRequest request) {
        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, request);
        request.setTicketTypes(new ArrayList<>(new HashSet<>(request.getTicketTypes())));
        var updatedGroupTicketTypes = service.update(
                (GroupTicketTypes) reflectionHelper.convertRequestDtoIntoEntityWhereNestedObjectsWithOnlyValidId(
                        request,
                        GroupTicketTypes.class
                )
        );
        var returnedGroupTicketTypes = modelMapper.map(updatedGroupTicketTypes, GroupTicketTypesDtoResponse.class);
        log.info(UPDATE_FINISH_MESSAGE, ENTITY_NAME, updatedGroupTicketTypes);
        return new ResponseEntity<>(returnedGroupTicketTypes, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<Page<GroupTicketTypesDtoResponse>> getByFilter(
            Principal user,
            @Valid @RequestBody GroupTicketTypesFilterDto filterDto,
            @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
            @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        log.debug(FIND_INIT_MESSAGE, ENTITY_NAME, page, size, filterDto);
        var pageable = createPageable(size, page, filterDto.getSortByFields(), filterDto.getSortDirection());
        var jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
        var accountId = jwtUser.getAccountId();
        var groupTicketTypesSpecification =
                specFactory.makeSpecificationFromEntityFilterDto(GroupTicketTypes.class, filterDto, accountId);
        var foundGroupTicketTypes = service.findAllByFilter(groupTicketTypesSpecification, pageable);
        var returnedTicketStatus = mapPage(foundGroupTicketTypes, GroupTicketTypesDtoResponse.class, pageable);
        log.debug(FIND_FINISH_MESSAGE, ENTITY_NAME, foundGroupTicketTypes.getTotalElements());
        return new ResponseEntity<>(returnedTicketStatus, HttpStatus.OK);
    }

    @GetMapping("/{id}")
    public ResponseEntity<GroupTicketTypesDtoResponse> getById(@PathVariable UUID id) {
        log.debug(FIND_BY_ID_INIT_MESSAGE, ENTITY_NAME, id);
        var foundGroupTicketTypes = service.findByIdAndAccountId(id);
        var returnedValue = modelMapper.map(foundGroupTicketTypes, GroupTicketTypesDtoResponse.class);
        log.debug(FIND_BY_ID_FINISH_MESSAGE, ENTITY_NAME, foundGroupTicketTypes);
        return new ResponseEntity<>(returnedValue, HttpStatus.OK);
    }
}
