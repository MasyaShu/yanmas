package ru.itterminal.yanmas.tickets.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.itterminal.yanmas.aau.controller.BaseControllerImpl;
import ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.tickets.model.TicketEvent;
import ru.itterminal.yanmas.tickets.model.dto.TicketEventDtoRequest;
import ru.itterminal.yanmas.tickets.model.dto.TicketEventDtoResponse;
import ru.itterminal.yanmas.tickets.model.dto.TicketEventFilterDto;
import ru.itterminal.yanmas.tickets.service.impl.TicketEventServiceImpl;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;
import java.util.List;
import java.util.UUID;

import static ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.EXIST_IN;

@SuppressWarnings({"unchecked", "rawtypes"})
@RestController("TicketEventControllerV1")
@Validated
@RequestMapping("api/v1/ticket/{id}/event")
@RequiredArgsConstructor
public class TicketEventControllerV1 extends BaseControllerImpl<
        TicketEvent,
        TicketEventServiceImpl,
        TicketEventDtoRequest,
        TicketEventDtoResponse,
        TicketEventFilterDto> {

    private static final Class responseClazz = TicketEventDtoResponse.class;
    private static final Class entityClazz = TicketEvent.class;

    @PostMapping()
    public ResponseEntity<TicketEventDtoResponse>
    create(@Validated(Create.class) @RequestBody TicketEventDtoRequest request,
            @PathVariable UUID id) {
        request.setTicketId(id);
        return create(request, entityClazz, responseClazz);
    }

    @GetMapping()
    public ResponseEntity<Page<TicketEventDtoResponse>>
    getByFilter(@Valid @RequestBody TicketEventFilterDto filter, @PathVariable UUID id,
                @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
                @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        var currentUser = getCurrentUser();
        var pageable = createPageable(size, page, filter.getSortByFields(), filter.getSortDirection());
        var ticketFilter = BaseEntityFilter.builder()
                .typeComparison(EXIST_IN.toString())
                .listOfIdEntities(List.of(id))
                .build();
        filter.setTicket(ticketFilter);
        var specification =
                specFactory
                        .makeSpecificationFromEntityFilterDto(entityClazz, filter, currentUser.getAccount().getId());
        var foundEntities = service.findAllByFilter(specification, pageable, currentUser, id);
        var returnedEntities = mapPage(foundEntities, responseClazz, pageable);
        return new ResponseEntity<>(returnedEntities, HttpStatus.OK);
    }
}
