package ru.itterminal.botdesk.tickets.controller;

import static java.lang.String.format;

import java.security.Principal;
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
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.service.impl.UserServiceImpl;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.model.spec.SpecificationsFactory;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.tickets.model.TicketSetting;
import ru.itterminal.botdesk.tickets.model.dto.TicketSettingDtoRequest;
import ru.itterminal.botdesk.tickets.model.dto.TicketSettingDtoResponse;
import ru.itterminal.botdesk.tickets.model.dto.TicketSettingFilterDto;
import ru.itterminal.botdesk.tickets.service.impl.TicketSettingServiceImpl;

@SuppressWarnings("DuplicatedCode")
@Slf4j
@RestController("TicketSettingControllerV1")
@Validated
@RequestMapping("api/v1/ticket-setting")
@RequiredArgsConstructor
public class TicketSettingControllerV1 extends BaseController {

    private final UserServiceImpl userService;
    private final TicketSettingServiceImpl ticketSettingService;
    private final SpecificationsFactory specFactory;
    public static final String FIND_BY_AUTHOR_ID_INIT_MESSAGE = "Get request for find {} by authorId: {}";
    public static final String FIND_BY_AUTHOR_ID_FINISH_MESSAGE = "Done find {} by authorId: {}";

    private final String ENTITY_NAME = TicketSetting.class.getSimpleName();

    @PostMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    public ResponseEntity<TicketSettingDtoResponse> create(Principal principal,
                                                           @Validated(Create.class) @RequestBody TicketSettingDtoRequest request) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, request);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        var createdTicketSetting = ticketSettingService.create(
                ticketSettingService.convertRequestDtoIntoEntityWithNestedObjectsWithOnlyId(request, jwtUser.getAccountId())
        );
        var returnedTicketSetting = modelMapper.map(createdTicketSetting, TicketSettingDtoResponse.class);
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
    public ResponseEntity<TicketSettingDtoResponse> update(Principal principal,
                                                           @Validated(Update.class) @RequestBody TicketSettingDtoRequest request) {
        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, request);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        var updatedTicketSetting = ticketSettingService.update(
                ticketSettingService.convertRequestDtoIntoEntityWithNestedObjectsWithOnlyId(request, jwtUser.getAccountId())
        );
        var returnedTicketSetting = modelMapper.map(updatedTicketSetting, TicketSettingDtoResponse.class);
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


    @GetMapping("/{id}")
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR', 'AUTHOR')")
    public ResponseEntity<TicketSettingDtoResponse> getById(@PathVariable UUID id) {
        log.debug(FIND_BY_ID_INIT_MESSAGE, ENTITY_NAME, id);
        var foundTicketSetting = ticketSettingService.findByIdAndAccountId(id);
        var returnedTicketSetting = modelMapper.map(foundTicketSetting, TicketSettingDtoResponse.class);
        log.debug(FIND_BY_ID_FINISH_MESSAGE, ENTITY_NAME, foundTicketSetting);
        return new ResponseEntity<>(returnedTicketSetting, HttpStatus.OK);
    }

    @GetMapping()
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR', 'AUTHOR')")
    public ResponseEntity<Page<TicketSettingDtoResponse>> getByFilter(
            Principal user,
            @Valid @RequestBody TicketSettingFilterDto filterDto,
            @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
            @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        log.debug(FIND_INIT_MESSAGE, ENTITY_NAME, page, size, filterDto);
        var pageable = createPageable(size, page, filterDto.getSortByFields(), filterDto.getSortDirection());
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
        var accountId = jwtUser.getAccountId();
        var ticketSettingSpecification =
                specFactory.makeSpecificationFromEntityFilterDto(TicketSetting.class, filterDto, accountId);
        var foundTicketSetting = ticketSettingService.findAllByFilter(ticketSettingSpecification, pageable);
        var returnedTicketSetting = mapPage(foundTicketSetting, TicketSettingDtoResponse.class, pageable);
        log.debug(FIND_FINISH_MESSAGE, ENTITY_NAME, foundTicketSetting.getTotalElements());
        return new ResponseEntity<>(returnedTicketSetting, HttpStatus.OK);
    }

    @GetMapping("/by-author/{authorId}")
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR', 'AUTHOR')")
    public ResponseEntity<TicketSettingDtoResponse> getSettingOrPredefinedValuesForTicket
            (Principal user, @PathVariable UUID authorId) {
        log.debug(FIND_BY_AUTHOR_ID_INIT_MESSAGE, ENTITY_NAME, authorId);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
        User foundUser = userService.findByIdAndAccountId(authorId);
        TicketSetting ticketSetting = ticketSettingService.getSettingOrPredefinedValuesForTicket(
                jwtUser.getAccountId(),
                foundUser.getGroup().getId(),
                authorId
        );
        var returnedTicketSetting = modelMapper.map(ticketSetting, TicketSettingDtoResponse.class);
        log.debug(FIND_BY_AUTHOR_ID_FINISH_MESSAGE, ENTITY_NAME, authorId);
        return new ResponseEntity<>(returnedTicketSetting, HttpStatus.OK);
    }


}
