package ru.itterminal.yanmas.tickets.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Page;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.itterminal.yanmas.aau.util.ReflectionHelper;
import ru.itterminal.yanmas.commons.controller.BaseController;
import ru.itterminal.yanmas.commons.model.filter.StringFilter;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.commons.model.validator.scenario.Update;
import ru.itterminal.yanmas.security.jwt.JwtUser;
import ru.itterminal.yanmas.tickets.model.TicketType;
import ru.itterminal.yanmas.tickets.model.dto.TicketTypeDto;
import ru.itterminal.yanmas.tickets.model.dto.TicketTypeFilterDto;
import ru.itterminal.yanmas.tickets.service.impl.SettingsAccessToTicketTypesServiceImpl;
import ru.itterminal.yanmas.tickets.service.impl.TicketTypeServiceImpl;
import ru.itterminal.yanmas.tickets.service.validator.TicketTypeOperationValidator;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;
import java.security.Principal;
import java.util.UUID;

import static ru.itterminal.yanmas.commons.model.filter.StringFilter.TypeComparisonForStringFilter.TEXT_EQUALS;

@Slf4j
@RestController("TicketTypeControllerV1")
@Validated
@RequestMapping("api/v1/ticket-type")
@RequiredArgsConstructor
public class TicketTypeControllerV1 extends BaseController {

    private final TicketTypeServiceImpl typeService;
    private final SettingsAccessToTicketTypesServiceImpl accessToTicketTypesService;
    private final TicketTypeOperationValidator validator;
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
        var permittedTicketTypes = accessToTicketTypesService.getPermittedTicketTypes(jwtUser.getId());
        if (permittedTicketTypes != null) {
            Specification<TicketType> specificationByListId = null;
            for (TicketType tt : permittedTicketTypes) {
                var filterByListId = StringFilter.builder()
                        .typeComparison(TEXT_EQUALS.toString())
                        .value(tt.getId().toString())
                        .build();
                if(specificationByListId == null) {
                    specificationByListId = specFactory.makeSpecification(TicketType.class, "id", filterByListId);
                } else {
                    var specificationById= specFactory.makeSpecification(TicketType.class, "id", filterByListId);
                    specificationByListId = specificationByListId.or(specificationById);
                }
            }
            ticketTypesSpecification = ticketTypesSpecification.and(specificationByListId);
        }
        foundTicketTypes = typeService.findAllByFilter(ticketTypesSpecification, pageable);
        returnedTicketTypes = mapPage(foundTicketTypes, TicketTypeDto.class, pageable);
        log.debug(FIND_FINISH_MESSAGE, ENTITY_NAME, foundTicketTypes.getTotalElements());
        return new ResponseEntity<>(returnedTicketTypes, HttpStatus.OK);
    }

    @GetMapping("/{id}")
    public ResponseEntity<TicketTypeDto> getById(@PathVariable UUID id) {
        log.debug(FIND_BY_ID_INIT_MESSAGE, ENTITY_NAME, id);
        var foundTicketType = typeService.findByIdAndAccountId(id);
        validator.checkAccessBeforeRead(foundTicketType);
        TicketTypeDto returnedTicketTypes = modelMapper.map(foundTicketType, TicketTypeDto.class);
        log.debug(FIND_BY_ID_FINISH_MESSAGE, ENTITY_NAME, foundTicketType);
        return new ResponseEntity<>(returnedTicketTypes, HttpStatus.OK);
    }
}
