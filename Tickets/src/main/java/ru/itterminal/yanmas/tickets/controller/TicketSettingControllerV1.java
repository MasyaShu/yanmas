package ru.itterminal.yanmas.tickets.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.itterminal.yanmas.aau.controller.BaseControllerImpl;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.commons.model.validator.scenario.Update;
import ru.itterminal.yanmas.tickets.model.TicketSetting;
import ru.itterminal.yanmas.tickets.model.dto.TicketSettingDtoRequest;
import ru.itterminal.yanmas.tickets.model.dto.TicketSettingDtoResponse;
import ru.itterminal.yanmas.tickets.model.dto.TicketSettingFilterDto;
import ru.itterminal.yanmas.tickets.service.impl.TicketSettingServiceImpl;

import javax.validation.Valid;
import javax.validation.constraints.Positive;
import javax.validation.constraints.PositiveOrZero;
import java.util.UUID;

@SuppressWarnings({"DuplicatedCode", "unchecked", "rawtypes"})
@RestController("TicketSettingControllerV1")
@Validated
@RequestMapping("api/v1/ticket/setting-initial")
@RequiredArgsConstructor
public class TicketSettingControllerV1 extends BaseControllerImpl<
        TicketSetting,
        TicketSettingServiceImpl,
        TicketSettingDtoRequest,
        TicketSettingDtoResponse,
        TicketSettingFilterDto> {

    private final TicketSettingServiceImpl ticketSettingService;

    private static final Class responseClazz = TicketSettingDtoResponse.class;
    private static final Class entityClazz = TicketSetting.class;

    @PostMapping()
    public ResponseEntity<TicketSettingDtoResponse>
    create(@Validated(Create.class) @RequestBody TicketSettingDtoRequest request) {
        return create(request, entityClazz, responseClazz);
    }

    @PutMapping()
    public ResponseEntity<TicketSettingDtoResponse>
    update(@Validated(Update.class) @RequestBody TicketSettingDtoRequest request) {
        return update(request, entityClazz, responseClazz);
    }

    @GetMapping()
    public ResponseEntity<Page<TicketSettingDtoResponse>>
    getByFilter(@Valid @RequestBody TicketSettingFilterDto filter,
                @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
                @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
        return getByFilter(filter, page, size, entityClazz, responseClazz);
    }

    @GetMapping("/{id}")
    public ResponseEntity<TicketSettingDtoResponse> getById(@PathVariable UUID id) {
        return getById(id, responseClazz);
    }


    @GetMapping("/by-author/{authorId}")
    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR', 'AUTHOR')")
    public ResponseEntity<TicketSettingDtoResponse> getSettingOrPredefinedValuesForTicket
            (@PathVariable UUID authorId) {
        var currentUser = getCurrentUser();
        var foundUser = userService.findByIdAndAccountId(authorId, currentUser);
        var ticketSetting = ticketSettingService.getSettingOrPredefinedValuesForTicket(
                currentUser,
                foundUser
        );
        var returnedTicketSetting = modelMapper.map(ticketSetting, TicketSettingDtoResponse.class);
        return new ResponseEntity<>(returnedTicketSetting, HttpStatus.OK);
    }


}
