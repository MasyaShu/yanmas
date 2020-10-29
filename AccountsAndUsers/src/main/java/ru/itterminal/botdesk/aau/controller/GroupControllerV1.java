package ru.itterminal.botdesk.aau.controller;

import lombok.extern.slf4j.Slf4j;
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
import org.springframework.web.bind.annotation.*;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.dto.GroupDto;
import ru.itterminal.botdesk.aau.model.dto.GroupFilterDto;
import ru.itterminal.botdesk.aau.model.spec.GroupSpec;
import ru.itterminal.botdesk.aau.service.impl.GroupServiceImpl;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;
import ru.itterminal.botdesk.jwt.JwtUser;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;
import java.security.Principal;
import java.util.UUID;

@Slf4j
@RestController("GroupControllerV1")
@Validated
@RequestMapping("api/v1/group")
public class GroupControllerV1 extends BaseController {

    GroupServiceImpl service;
    GroupSpec spec;

    @Autowired
    public GroupControllerV1(GroupServiceImpl service, GroupSpec userSpec) {
        this.spec = userSpec;
        this.service = service;
    }

    private final String ENTITY_NAME = Group.class.getSimpleName();

    @PostMapping()
    @ResponseStatus(value = HttpStatus.CREATED)
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<GroupDto> create(
            @Validated(Create.class) @RequestBody GroupDto request) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, request);
        Group group = modelMapper.map(request, Group.class);
        Group createdGroup = service.create(group);
        GroupDto returnedGroup =
                modelMapper.map(createdGroup, GroupDto.class);
        log.info(CREATE_FINISH_MESSAGE, ENTITY_NAME, createdGroup);
        return new ResponseEntity<>(returnedGroup, HttpStatus.CREATED);
    }

    @PutMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR')")
    public ResponseEntity<GroupDto> update(
            @Validated(Update.class) @RequestBody GroupDto request) {
        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, request);
        Group user = modelMapper.map(request, Group.class);
        Group updatedGroup = service.update(user);
        GroupDto returnedGroup =
                modelMapper.map(updatedGroup, GroupDto.class);
        log.info(UPDATE_FINISH_MESSAGE, ENTITY_NAME, updatedGroup);
        return new ResponseEntity<>(returnedGroup, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<Page<GroupDto>> getByFilter(
            Principal user,
            @Valid @RequestBody GroupFilterDto filter,
            @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
            @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        log.debug(FIND_INIT_MESSAGE, ENTITY_NAME, page, size, filter);
        if (filter.getDirection() == null) {
            filter.setDirection("ASC");
        }
        if (filter.getSortBy() == null) {
            filter.setSortBy("name");
        }
        if (filter.getDeleted() == null) {
            filter.setDeleted("all");
        }
        Pageable pageable =
                PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(filter.getDirection()),
                        filter.getSortBy()));
        Page<Group> foundGroups;
        Page<GroupDto> returnedGroups;
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
        Specification<Group> groupSpecification = Specification
                .where(filter.getName() == null ? null : spec.getGroupByNameSpec(filter.getName()))
                .and(filter.getComment() == null ? null : spec.getGroupByCommentSpec(filter.getComment()))
                .and(filter.getIsDeprecated() == null ? null : spec.getGroupByIsDeprecatedSpec(filter.getIsDeprecated()))
                .and(filter.getIsInner() == null ? null : spec.getGroupByIsInnerSpec(filter.getIsInner()))
                .and(spec.getEntityByDeletedSpec(BaseFilterDto.FilterByDeleted.fromString(filter.getDeleted())))
                .and(spec.getGroupByAccountSpec(jwtUser.getAccountId()))
                .and(jwtUser.isInnerGroup() ? null : spec.getGroupByGroupSpec(jwtUser.getGroupId()));

        foundGroups = service.findAllByFilter(groupSpecification, pageable);
        returnedGroups = mapPage(foundGroups, GroupDto.class, pageable);
        log.debug(FIND_FINISH_MESSAGE, ENTITY_NAME, foundGroups.getTotalElements());
        return new ResponseEntity<>(returnedGroups, HttpStatus.OK);
    }
    @GetMapping("/{id}")
    public ResponseEntity<GroupDto> getById(Principal user, @PathVariable UUID id) {
        log.debug(FIND_BY_ID_INIT_MESSAGE, ENTITY_NAME, id);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
        Group foundGroup;
        if (jwtUser.isInnerGroup()) {
             foundGroup = service.findByIdAndAccountId(id, jwtUser.getAccountId());
        } else {
            foundGroup = service.findByIdAndAccountIdAndOwnGroupId(id, jwtUser.getAccountId(), jwtUser.getGroupId());
        }
        GroupDto returnedGroup = modelMapper.map(foundGroup, GroupDto.class);
        log.debug(FIND_BY_ID_FINISH_MESSAGE, ENTITY_NAME, foundGroup);
        return new ResponseEntity<>(returnedGroup, HttpStatus.OK);
    }

    @DeleteMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    ResponseEntity<Void> physicalDelete(@RequestBody GroupDto request) {
        throw new UnsupportedOperationException("Physical delete will be implement in the further");
    }
}
