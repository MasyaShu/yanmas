package ru.itterminal.botdesk.tickets.controller;

import static java.lang.String.format;

import java.security.Principal;
import java.util.List;
import java.util.UUID;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.GroupServiceImpl;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.tickets.model.TicketSetting;
import ru.itterminal.botdesk.tickets.model.dto.TicketSettingDto;
import ru.itterminal.botdesk.tickets.model.dto.TicketSettingFilterDto;
import ru.itterminal.botdesk.tickets.service.impl.TicketSettingServiceImpl;
import ru.itterminal.botdesk.tickets.service.impl.TicketStatusServiceImpl;
import ru.itterminal.botdesk.tickets.service.impl.TicketTypeServiceImpl;

@Slf4j
@RestController("TicketSettingControllerV1")
@Validated
@RequestMapping("api/v1/ticketSetting")
@AllArgsConstructor
public class TicketSettingControllerV1 extends BaseController {

    private final AccountServiceImpl accountService;
    private final GroupServiceImpl groupService;
    private final UserServiceImpl userService;
    private final TicketStatusServiceImpl ticketStatusService;
    private final TicketTypeServiceImpl ticketTypeService;
    private final TicketSettingServiceImpl ticketSettingService;

    private final String ENTITY_NAME = TicketSetting.class.getSimpleName();

    @PostMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<TicketSettingDto> create(Principal principal,
                                                   @Validated(Create.class) @RequestBody TicketSettingDto request) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, request);
        TicketSetting ticketSetting = modelMapper.map(request, TicketSetting.class);

        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        ticketSetting.setAccount(accountService.findById(jwtUser.getAccountId()));
        UUID accountId = ticketSetting.getAccount().getId();

        ticketSetting.setGroup(groupService.findByIdAndAccountId(request.getGroupId(), accountId));
        ticketSetting.setAuthor(userService.findByIdAndAccountId(request.getAuthorId(), accountId));

        List<User> observers = userService.findAllByAccountIdAndListId(accountId, request.getObserversId());
        ticketSetting.setObservers(observers);

        List<User> executors = userService.findAllByAccountIdAndListId(accountId, request.getExecutorsId());
        ticketSetting.setObservers(executors);

        ticketSetting.setTicketTypeForNew(
                ticketTypeService.findByIdAndAccountId(request.getTicketTypeIdForNew(), accountId)
        );

        ticketSetting.setTicketStatusForNew(
                ticketStatusService.findByIdAndAccountId(request.getTicketStatusIdForNew(), accountId)
        );

        ticketSetting.setTicketStatusForReopen(
                ticketStatusService.findByIdAndAccountId(request.getTicketStatusIdForReopen(), accountId)
        );
        ticketSetting.setTicketStatusForClose(
                ticketStatusService.findByIdAndAccountId(request.getTicketStatusIdForClose(), accountId)
        );
        ticketSetting.setTicketStatusForCancel(
                ticketStatusService.findByIdAndAccountId(request.getTicketStatusIdForCancel(), accountId)
        );

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

    @PutMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<TicketSettingDto> update(Principal principal,
                                                  @Validated(Update.class) @RequestBody TicketSettingDto request) {
        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, request);
        TicketSetting ticketSetting = modelMapper.map(request, TicketSetting.class);

        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        ticketSetting.setAccount(accountService.findById(jwtUser.getAccountId()));
        UUID accountId = ticketSetting.getAccount().getId();

        ticketSetting.setGroup(groupService.findByIdAndAccountId(request.getGroupId(), accountId));
        ticketSetting.setAuthor(userService.findByIdAndAccountId(request.getAuthorId(), accountId));

        List<User> observers = userService.findAllByAccountIdAndListId(accountId, request.getObserversId());
        ticketSetting.setObservers(observers);

        List<User> executors = userService.findAllByAccountIdAndListId(accountId, request.getExecutorsId());
        ticketSetting.setObservers(executors);

        ticketSetting.setTicketTypeForNew(
                ticketTypeService.findByIdAndAccountId(request.getTicketTypeIdForNew(), accountId)
        );

        ticketSetting.setTicketStatusForNew(
                ticketStatusService.findByIdAndAccountId(request.getTicketStatusIdForNew(), accountId)
        );

        ticketSetting.setTicketStatusForReopen(
                ticketStatusService.findByIdAndAccountId(request.getTicketStatusIdForReopen(), accountId)
        );
        ticketSetting.setTicketStatusForClose(
                ticketStatusService.findByIdAndAccountId(request.getTicketStatusIdForClose(), accountId)
        );
        ticketSetting.setTicketStatusForCancel(
                ticketStatusService.findByIdAndAccountId(request.getTicketStatusIdForCancel(), accountId)
        );

        TicketSetting updatedTicketSetting = ticketSettingService.update(ticketSetting);
        TicketSettingDto returnedTicketSetting = modelMapper.map(updatedTicketSetting, TicketSettingDto.class);
        log.info(UPDATE_FINISH_MESSAGE, ENTITY_NAME, updatedTicketSetting);
        return new ResponseEntity<>(returnedTicketSetting, HttpStatus.OK);
    }

    @PutMapping("/check-access")
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<String> updateCheckAccess() {
        String message = format(SUCCESSFUL_CHECK_ACCESS, WORD_UPDATE, ENTITY_NAME);
        log.trace(message);
        return ResponseEntity.ok(message);
    }

//    @GetMapping()
//    public ResponseEntity<Page<TicketSettingDto>> getByFilter(
//            Principal user,
//            @Valid @RequestBody TicketSettingFilterDto filter,
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
