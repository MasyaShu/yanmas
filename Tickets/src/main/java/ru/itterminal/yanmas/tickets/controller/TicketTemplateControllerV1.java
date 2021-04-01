package ru.itterminal.yanmas.tickets.controller;

import java.security.Principal;
import java.util.UUID;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;

import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.AccessDeniedException;
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
import ru.itterminal.yanmas.aau.util.ReflectionHelper;
import ru.itterminal.yanmas.commons.controller.BaseController;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.commons.model.validator.scenario.Update;
import ru.itterminal.yanmas.security.jwt.JwtUser;
import ru.itterminal.yanmas.tickets.model.TicketTemplate;
import ru.itterminal.yanmas.tickets.model.dto.TicketTemplateDtoRequest;
import ru.itterminal.yanmas.tickets.model.dto.TicketTemplateDtoResponse;
import ru.itterminal.yanmas.tickets.model.dto.TicketTemplateFilterDto;
import ru.itterminal.yanmas.tickets.service.impl.TicketTemplateServiceImpl;

@Slf4j
@RestController("TicketTemplateControllerV1")
@RequestMapping("api/v1/ticket-template")
@RequiredArgsConstructor
public class TicketTemplateControllerV1 extends BaseController {

    public static final String ACCESS_IS_DENIED = "Access is denied";

    private final TicketTemplateServiceImpl templateService;
    private final SpecificationsFactory specFactory;
    private final ReflectionHelper reflectionHelper;

    private final String ENTITY_NAME = TicketTemplate.class.getSimpleName();

    @PostMapping()
    public ResponseEntity<TicketTemplateDtoResponse> create(
            @Validated(Create.class) @RequestBody TicketTemplateDtoRequest request) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, request);
        TicketTemplate createdTicketTemplate = templateService.create(
                (TicketTemplate) reflectionHelper.convertRequestDtoIntoEntityWhereNestedObjectsWithOnlyValidId(
                        request,
                        TicketTemplate.class
                )
        );
        TicketTemplateDtoResponse returnedTicketTemplate =
                modelMapper.map(createdTicketTemplate, TicketTemplateDtoResponse.class);
        log.info(CREATE_FINISH_MESSAGE, ENTITY_NAME, request);
        return new ResponseEntity<>(returnedTicketTemplate, HttpStatus.CREATED);
    }

    @PutMapping()
    public ResponseEntity<TicketTemplateDtoResponse> update(
            @Validated(Update.class) @RequestBody TicketTemplateDtoRequest request) {
        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, request);
        TicketTemplate updatedTicketStatus = templateService.update(
                (TicketTemplate) reflectionHelper.convertRequestDtoIntoEntityWhereNestedObjectsWithOnlyValidId(
                        request,
                        TicketTemplate.class
                )
        );
        TicketTemplateDtoResponse returnedTicketTemplate =
                modelMapper.map(updatedTicketStatus, TicketTemplateDtoResponse.class);
        log.info(UPDATE_FINISH_MESSAGE, ENTITY_NAME, updatedTicketStatus);
        return new ResponseEntity<>(returnedTicketTemplate, HttpStatus.OK);
    }

    @GetMapping("/{id}")
    public ResponseEntity<TicketTemplateDtoResponse> getById(@PathVariable UUID id) {
        JwtUser jwtUser = (JwtUser) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        if (!jwtUser.isInnerGroup()) {
            throw new AccessDeniedException(ACCESS_IS_DENIED);
        }
        log.debug(FIND_BY_ID_INIT_MESSAGE, ENTITY_NAME, id);
        var foundTicketTemplate = templateService.findByIdAndAccountId(id);
        var returnedTemplateStatus = modelMapper.map(foundTicketTemplate, TicketTemplateDtoResponse.class);
        log.debug(FIND_BY_ID_FINISH_MESSAGE, ENTITY_NAME, foundTicketTemplate);
        return new ResponseEntity<>(returnedTemplateStatus, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<Page<TicketTemplateDtoResponse>> getByFilter(
            Principal user,
            @Valid @RequestBody TicketTemplateFilterDto filterDto,
            @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
            @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
        if (!jwtUser.isInnerGroup()) {
            throw new AccessDeniedException(ACCESS_IS_DENIED);
        }
        log.debug(FIND_INIT_MESSAGE, ENTITY_NAME, page, size, filterDto);
        var pageable = createPageable(size, page, filterDto.getSortByFields(), filterDto.getSortDirection());
        var accountId = jwtUser.getAccountId();
        var ticketTemplateSpecification =
                specFactory.makeSpecificationFromEntityFilterDto(TicketTemplate.class, filterDto, accountId);
        var foundTicketTemplate = templateService.findAllByFilter(ticketTemplateSpecification, pageable);
        var returnedTicketTemplate = mapPage(foundTicketTemplate, TicketTemplateDtoResponse.class, pageable);
        log.debug(FIND_FINISH_MESSAGE, ENTITY_NAME, foundTicketTemplate.getTotalElements());
        return new ResponseEntity<>(returnedTicketTemplate, HttpStatus.OK);
    }
}
