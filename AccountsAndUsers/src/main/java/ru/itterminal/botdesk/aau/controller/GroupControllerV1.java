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
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.dto.GroupDto;
import ru.itterminal.botdesk.aau.model.dto.GroupFilterDto;
import ru.itterminal.botdesk.aau.model.spec.GroupSpec;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.GroupServiceImpl;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;
import ru.itterminal.botdesk.security.jwt.JwtUser;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;
import java.security.Principal;
import java.util.UUID;

import static java.lang.String.format;


@Slf4j
@RestController("GroupControllerV1")
@Validated
@RequestMapping("api/v1/group")
public class GroupControllerV1 extends BaseController {

    private final GroupServiceImpl service;
    private final GroupSpec spec;
    private final AccountServiceImpl accountService;

    @Autowired
    public GroupControllerV1(GroupServiceImpl service, GroupSpec userSpec, AccountServiceImpl accountService) {
        this.spec = userSpec;
        this.service = service;
        this.accountService = accountService;
    }

    private final String ENTITY_NAME = Group.class.getSimpleName();

    @PostMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<GroupDto> create(Principal principal,
            @Validated(Create.class) @RequestBody GroupDto request) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, request);
        Group group = modelMapper.map(request, Group.class);
        group.setDeleted(false);
        group.setIsDeprecated(false);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        group.setAccount(accountService.findById(jwtUser.getAccountId()));
        Group createdGroup = service.create(group);
        GroupDto returnedGroup =
                modelMapper.map(createdGroup, GroupDto.class);
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
        Group group = modelMapper.map(request, Group.class);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        group.setAccount(accountService.findById(jwtUser.getAccountId()));
        Group updatedGroup = service.update(group);
        GroupDto returnedGroup =
                modelMapper.map(updatedGroup, GroupDto.class);
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
                .and(spec.getEntityByAccountSpec(jwtUser.getAccountId()))
                .and(jwtUser.isInnerGroup() ? null : spec.getGroupByGroupSpec(jwtUser.getGroupId()))
                .and(filter.getOutId() == null ? null :  spec.getEntityByOutIdSpec(filter.getOutId()));

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
        foundGroup = service.findByIdAndAccountId(id, jwtUser.getAccountId());
        GroupDto returnedGroup = modelMapper.map(foundGroup, GroupDto.class);
        log.debug(FIND_BY_ID_FINISH_MESSAGE, ENTITY_NAME, foundGroup);
        return new ResponseEntity<>(returnedGroup, HttpStatus.OK);
    }

    @DeleteMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<Void> physicalDelete() {
        throw new UnsupportedOperationException("Physical delete will be implement in the further");
    }

    private void isUserInInnerGroup(){
        JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (!jwtUser.isInnerGroup()) {
            throw new AccessDeniedException("access is denied");
        }
    }
}
