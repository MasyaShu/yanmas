package ru.itterminal.botdesk.aau.controller;

import static java.lang.String.format;
import static ru.itterminal.botdesk.commons.model.filter.StringFilter.TypeComparisonForStringFilter.TEXT_EQUALS;

import java.security.Principal;
import java.util.UUID;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;

import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
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
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.dto.GroupDto;
import ru.itterminal.botdesk.aau.model.dto.GroupFilterDto;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.GroupServiceImpl;
import ru.itterminal.botdesk.aau.service.validator.GroupOperationValidator;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.model.filter.StringFilter;
import ru.itterminal.botdesk.commons.model.spec.SpecificationsFactory;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;
import ru.itterminal.botdesk.security.jwt.JwtUser;

@Slf4j
@RestController("GroupControllerV1")
@Validated
@RequestMapping("api/v1/group")
@RequiredArgsConstructor
public class GroupControllerV1 extends BaseController {

    private final AccountServiceImpl accountService;
    private final GroupServiceImpl service;
    private final GroupOperationValidator validator;
    private final SpecificationsFactory specFactory;

    private final String ENTITY_NAME = Group.class.getSimpleName();

    @PostMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<GroupDto> create(Principal principal,
                                           @Validated(Create.class) @RequestBody GroupDto request) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, request);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        var group = convertRequestDtoIntoEntity(request, jwtUser.getAccountId());
        var createdGroup = service.create(group);
        var returnedGroup = modelMapper.map(createdGroup, GroupDto.class);
        log.info(CREATE_FINISH_MESSAGE, ENTITY_NAME, createdGroup);
        return new ResponseEntity<>(returnedGroup, HttpStatus.CREATED);
    }

    @PostMapping("/check-access")
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<String> createCheckAccess() {
        isUserInInnerGroup();
        String message = format(SUCCESSFUL_CHECK_ACCESS, WORD_CREATE, ENTITY_NAME);
        log.trace(message);
        return ResponseEntity.ok(message);
    }

    @PutMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR')")
    public ResponseEntity<GroupDto> update(Principal principal,
                                           @Validated(Update.class) @RequestBody GroupDto request) {
        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, request);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        var group = convertRequestDtoIntoEntity(request, jwtUser.getAccountId());
        var updatedGroup = service.update(group);
        var returnedGroup = modelMapper.map(updatedGroup, GroupDto.class);
        log.info(UPDATE_FINISH_MESSAGE, ENTITY_NAME, updatedGroup);
        return new ResponseEntity<>(returnedGroup, HttpStatus.OK);
    }

    @PutMapping("/check-access")
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR')")
    public ResponseEntity<String> updateCheckAccess() {
        isUserInInnerGroup();
        String message = format(SUCCESSFUL_CHECK_ACCESS, WORD_UPDATE, ENTITY_NAME);
        log.trace(message);
        return ResponseEntity.ok(message);
    }

    @GetMapping()
    public ResponseEntity<Page<GroupDto>> getByFilter(
            Principal user,
            @Valid @RequestBody GroupFilterDto filterDto,
            @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
            @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        log.debug(FIND_INIT_MESSAGE, ENTITY_NAME, page, size, filterDto);
        var pageable = createPageable(size, page, filterDto.getSortByFields(), filterDto.getSortDirection());
        Page<Group> foundGroups;
        Page<GroupDto> returnedGroups;
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
        var accountId = jwtUser.getAccountId();
        var groupSpecification = specFactory.makeSpecificationFromEntityFilterDto(Group.class, filterDto, accountId);
        if (!jwtUser.isInnerGroup()) {
            var filterById = StringFilter.builder()
                    .typeComparison(TEXT_EQUALS.toString())
                    .value(jwtUser.getGroupId().toString())
                    .build();
            var specificationById = specFactory.makeSpecification(Group.class, "id", filterById);
            groupSpecification = groupSpecification.and(specificationById);
        }
        foundGroups = service.findAllByFilter(groupSpecification, pageable);
        returnedGroups = mapPage(foundGroups, GroupDto.class, pageable);
        log.debug(FIND_FINISH_MESSAGE, ENTITY_NAME, foundGroups.getTotalElements());
        return new ResponseEntity<>(returnedGroups, HttpStatus.OK);
    }

    @GetMapping("/{id}")
    public ResponseEntity<GroupDto> getById(@PathVariable UUID id) {
        log.debug(FIND_BY_ID_INIT_MESSAGE, ENTITY_NAME, id);
        var foundGroup = service.findByIdAndAccountId(id);
        validator.checkAccessBeforeRead(foundGroup);
        var returnedGroup = modelMapper.map(foundGroup, GroupDto.class);
        log.debug(FIND_BY_ID_FINISH_MESSAGE, ENTITY_NAME, foundGroup);
        return new ResponseEntity<>(returnedGroup, HttpStatus.OK);
    }

    private void isUserInInnerGroup() {
        JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (!jwtUser.isInnerGroup()) {
            throw new AccessDeniedException("access is denied");
        }
    }

    private Group convertRequestDtoIntoEntity(GroupDto request, UUID accountId) {
        var group = modelMapper.map(request, Group.class);
        // !!! setting is here because it is not possible into service layer (cycle dependency with AccountServiceImpl)
        group.setAccount(accountService.findById(accountId));
        return group;
    }
}
