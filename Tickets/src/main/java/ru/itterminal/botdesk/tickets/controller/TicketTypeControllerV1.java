package ru.itterminal.botdesk.tickets.controller;

import java.security.Principal;
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
import ru.itterminal.botdesk.aau.util.ReflectionHelper;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.model.spec.SpecificationsFactory;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;
import ru.itterminal.botdesk.security.jwt.JwtUser;
import ru.itterminal.botdesk.tickets.model.TicketType;
import ru.itterminal.botdesk.tickets.model.dto.TicketTypeDto;
import ru.itterminal.botdesk.tickets.model.dto.TicketTypeFilterDto;
import ru.itterminal.botdesk.tickets.service.impl.TicketTypeServiceImpl;

@Slf4j
@RestController("TicketTypeControllerV1")
@Validated
@RequestMapping("api/v1/ticket-type")
@RequiredArgsConstructor
public class TicketTypeControllerV1 extends BaseController {

    private final TicketTypeServiceImpl typeService;
    private final SpecificationsFactory specFactory;
    private final ReflectionHelper reflectionHelper;

    private final String ENTITY_NAME = TicketType.class.getSimpleName();

    @PostMapping()
    public ResponseEntity<TicketTypeDto> create(@Validated(Create.class) @RequestBody TicketTypeDto request) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, request);
        var ticketType = (TicketType) reflectionHelper.convertRequestDtoIntoEntityWhereNestedObjectsWithOnlyValidId(
                request,
                TicketType.class
        );
        ticketType.setIsPredefinedForNewTicket(false);
        var createdTicketType = typeService.create(ticketType);
        var returnedTicketTypes = modelMapper.map(createdTicketType, TicketTypeDto.class);
        log.info(CREATE_FINISH_MESSAGE, ENTITY_NAME, createdTicketType);
        return new ResponseEntity<>(returnedTicketTypes, HttpStatus.CREATED);
    }

    @PutMapping()
    public ResponseEntity<TicketTypeDto> update(@Validated(Update.class) @RequestBody TicketTypeDto request) {
        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, request);
        TicketType updatedTicketType = typeService.update(
                (TicketType) reflectionHelper.convertRequestDtoIntoEntityWhereNestedObjectsWithOnlyValidId(
                        request,
                        TicketType.class
                )
        );
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
        var ticketTypesSpecification =
                specFactory.makeSpecificationFromEntityFilterDto(TicketType.class, filterDto, accountId);
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
