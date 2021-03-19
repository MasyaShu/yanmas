package ru.itterminal.botdesk.tickets.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.model.spec.SpecificationsFactory;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.model.dto.TicketTypeDto;
import ru.itterminal.botdesk.tickets.model.dto.TicketTypeFilterDto;
import ru.itterminal.botdesk.tickets.service.impl.TicketTypeServiceImpl;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;
import java.security.Principal;
import java.util.UUID;

@Slf4j
@RestController("TicketTypeControllerV1")
@Validated
@RequestMapping("api/v1/ticket-type")
@RequiredArgsConstructor
public class TicketTypeControllerV1 extends BaseController {

    private final TicketTypeServiceImpl typeService;
    private final SpecificationsFactory specFactory;

    private final String ENTITY_NAME = TicketType.class.getSimpleName();

    @PostMapping()
    public ResponseEntity<TicketTypeDto> create(Principal principal,
                                                @Validated(Create.class) @RequestBody TicketTypeDto request) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, request);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        TicketType ticketType = typeService.convertRequestDtoIntoEntityWithNestedObjectsWithOnlyId(request, jwtUser.getAccountId());
        ticketType.setIsPredefinedForNewTicket(false);
        TicketType createdTicketType = typeService.create(ticketType);
        TicketTypeDto returnedTicketTypes = modelMapper.map(createdTicketType, TicketTypeDto.class);
        log.info(CREATE_FINISH_MESSAGE, ENTITY_NAME, createdTicketType);
        return new ResponseEntity<>(returnedTicketTypes, HttpStatus.CREATED);
    }

    @PutMapping()
    public ResponseEntity<TicketTypeDto> update(Principal principal,
                                                @Validated(Update.class) @RequestBody TicketTypeDto request) {
        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, request);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        TicketType ticketType = typeService.convertRequestDtoIntoEntityWithNestedObjectsWithOnlyId(request, jwtUser.getAccountId());
        TicketType updatedTicketType = typeService.update(ticketType);
        TicketTypeDto returnedTicketTypes = modelMapper.map(updatedTicketType, TicketTypeDto.class);
        log.info(UPDATE_FINISH_MESSAGE, ENTITY_NAME, updatedTicketType);
        return new ResponseEntity<>(returnedTicketTypes, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<Page<TicketTypeDto>> getByFilter(
            Principal user,
            @Valid @RequestBody TicketTypeFilterDto filterDto,
            @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
            @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        log.debug(FIND_INIT_MESSAGE, ENTITY_NAME, page, size, filterDto);
        var pageable = createPageable(size, page, filterDto.getSortByFields(), filterDto.getSortDirection());
        Page<TicketType> foundTicketTypes;
        Page<TicketTypeDto> returnedTicketTypes;
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
        var accountId = jwtUser.getAccountId();
        var ticketTypesSpecification = specFactory.makeSpecificationFromEntityFilterDto(TicketType.class, filterDto, accountId);
        foundTicketTypes = typeService.findAllByFilter(ticketTypesSpecification, pageable);
        returnedTicketTypes = mapPage(foundTicketTypes, TicketTypeDto.class, pageable);
        log.debug(FIND_FINISH_MESSAGE, ENTITY_NAME, foundTicketTypes.getTotalElements());
        return new ResponseEntity<>(returnedTicketTypes, HttpStatus.OK);
    }

    @GetMapping("/{id}")
    public ResponseEntity<TicketTypeDto> getById(@PathVariable UUID id) {
        log.debug(FIND_BY_ID_INIT_MESSAGE, ENTITY_NAME, id);
        var foundTicketType = typeService.findByIdAndAccountId(id);
        TicketTypeDto returnedTicketTypes = modelMapper.map(foundTicketType, TicketTypeDto.class);
        log.debug(FIND_BY_ID_FINISH_MESSAGE, ENTITY_NAME, foundTicketType);
        return new ResponseEntity<>(returnedTicketTypes, HttpStatus.OK);
    }
}
