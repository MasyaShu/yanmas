package ru.itterminal.botdesk.tickets.controller;

import static java.lang.String.format;

import java.security.Principal;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.GroupServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.tickets.model.TicketSetting;
import ru.itterminal.botdesk.tickets.model.dto.TicketSettingDto;
import ru.itterminal.botdesk.tickets.service.impl.TicketSettingServiceImpl;
import ru.itterminal.botdesk.tickets.service.impl.TicketStatusServiceImpl;
import ru.itterminal.botdesk.tickets.service.impl.TicketTypeServiceImpl;

@Slf4j
@RestController("TicketSettingControllerV1")
@Validated
@RequestMapping("api/v1/ticketSetting")
public class TicketSettingControllerV1 extends BaseController {

    private final AccountServiceImpl accountService;
    private final GroupServiceImpl groupService;
    private final UserServiceImpl userService;
    private final TicketStatusServiceImpl ticketStatusService;
    private final TicketTypeServiceImpl ticketTypeService;
    private final TicketSettingServiceImpl ticketSettingService;

    private final String ENTITY_NAME = TicketSetting.class.getSimpleName();

    public TicketSettingControllerV1(AccountServiceImpl accountService,
                                     GroupServiceImpl groupService,
                                     UserServiceImpl userService,
                                     TicketStatusServiceImpl ticketStatusService,
                                     TicketTypeServiceImpl ticketTypeService,
                                     TicketSettingServiceImpl ticketSettingService) {
        this.accountService = accountService;
        this.groupService = groupService;
        this.userService = userService;
        this.ticketStatusService = ticketStatusService;
        this.ticketTypeService = ticketTypeService;
        this.ticketSettingService = ticketSettingService;
    }

    @PostMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<TicketSettingDto> create(Principal principal,
                                                   @Validated(Create.class) @RequestBody TicketSettingDto request) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, request);
        TicketSetting ticketSetting = modelMapper.map(request, TicketSetting.class);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        ticketSetting.setAccount(accountService.findById(jwtUser.getAccountId()));
        ticketSetting.setGroup(groupService.findById(request.getGroupId()));
        ticketSetting.setAuthor(userService.findById(request.getAuthorId()));

        UUID accountId = ticketSetting.getAccount().getId();
        if (ticketSetting.getObservers()!=null && !ticketSetting.getObservers().isEmpty()) {
            List<User> observers = new ArrayList<>();
            observers = request.getObserversId().stream()
                    .map(id -> userService.findByIdAndAccountId(id, accountId))
                    .collect(Collectors.toList());
            ticketSetting.setObservers(observers);
        }

        ticketSetting.setDeleted(false);
        TicketSetting createdTicketSetting = ticketSettingService.create(ticketSetting);
        TicketSettingDto returnedTicketSetting = modelMapper.map(createdTicketSetting, TicketSettingDto.class);
        log.info(CREATE_FINISH_MESSAGE, ENTITY_NAME, createdTicketSetting);
        return new ResponseEntity<>(returnedTicketSetting, HttpStatus.CREATED);
    }

    @PostMapping("/check-access")
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<String> createCheckAccess() {
        String message = format(SUCCESSFUL_CHECK_ACCESS, WORD_CREATE, ENTITY_NAME);
        log.trace(message);
        return ResponseEntity.ok(message);
    }

//    @PutMapping()
//    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
//    public ResponseEntity<TicketStatusDto> update(Principal principal,
//                                                  @Validated(Update.class) @RequestBody TicketStatusDto request) {
//        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, request);
//        TicketStatus ticketType = modelMapper.map(request, TicketStatus.class);
//        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
//        ticketType.setAccount(accountService.findById(jwtUser.getAccountId()));
//        TicketStatus updatedTicketStatus = service.update(ticketType);
//        TicketStatusDto returnedTicketStatus =
//                modelMapper.map(updatedTicketStatus, TicketStatusDto.class);
//        log.info(UPDATE_FINISH_MESSAGE, ENTITY_NAME, updatedTicketStatus);
//        return new ResponseEntity<>(returnedTicketStatus, HttpStatus.OK);
//    }
//
//    @PutMapping("/check-access")
//    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
//    public ResponseEntity<String> updateCheckAccess() {
//        String message = format(SUCCESSFUL_CHECK_ACCESS, WORD_UPDATE, ENTITY_NAME);
//        log.trace(message);
//        return ResponseEntity.ok(message);
//    }
//
//    @GetMapping()
//    public ResponseEntity<Page<TicketStatusDto>> getByFilter(
//            Principal user,
//            @Valid @RequestBody TicketStatusFilterDto filter,
//            @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
//            @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
//        log.debug(FIND_INIT_MESSAGE, ENTITY_NAME, page, size, filter);
//        if (filter.getDirection() == null) {
//            filter.setDirection("ASC");
//        }
//        if (filter.getSortBy() == null) {
//            filter.setSortBy("sortIndex");
//        }
//        if (filter.getDeleted() == null) {
//            filter.setDeleted("all");
//        }
//        Pageable pageable =
//                PageRequest.of(page, size, Sort.by(
//                        Sort.Direction.fromString(filter.getDirection()),
//                        filter.getSortBy()
//                ));
//        Page<TicketStatus> foundTicketStatus;
//        Page<TicketStatusDto> returnedTicketStatus;
//        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
//        Specification<TicketStatus> ticketTypesSpecification = Specification
//                .where(filter.getName() == null ? null : spec.getTicketStatusByNameSpec(filter.getName()))
//                .and(spec.getEntityByDeletedSpec(BaseFilterDto.FilterByDeleted.fromString(filter.getDeleted())))
//                .and(spec.getEntityByAccountSpec(jwtUser.getAccountId()))
//                .and(filter.getOutId() == null ? null : spec.getEntityByOutIdSpec(filter.getOutId()));
//
//        foundTicketStatus = service.findAllByFilter(ticketTypesSpecification, pageable);
//        returnedTicketStatus = mapPage(foundTicketStatus, TicketStatusDto.class, pageable);
//        log.debug(FIND_FINISH_MESSAGE, ENTITY_NAME, foundTicketStatus.getTotalElements());
//        return new ResponseEntity<>(returnedTicketStatus, HttpStatus.OK);
//    }
//
//    @GetMapping("/{id}")
//    public ResponseEntity<TicketStatusDto> getById(Principal user, @PathVariable UUID id) {
//        log.debug(FIND_BY_ID_INIT_MESSAGE, ENTITY_NAME, id);
//        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
//        TicketStatus foundTicketStatus;
//        foundTicketStatus = service.findByIdAndAccountId(id, jwtUser.getAccountId());
//        TicketStatusDto returnedTicketStatus = modelMapper.map(foundTicketStatus, TicketStatusDto.class);
//        log.debug(FIND_BY_ID_FINISH_MESSAGE, ENTITY_NAME, foundTicketStatus);
//        return new ResponseEntity<>(returnedTicketStatus, HttpStatus.OK);
//    }
}
