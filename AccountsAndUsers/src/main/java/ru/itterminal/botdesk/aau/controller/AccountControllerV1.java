package ru.itterminal.botdesk.aau.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.dto.AccountCreateDto;
import ru.itterminal.botdesk.aau.model.dto.AccountDto;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;
import ru.itterminal.botdesk.security.jwt.JwtUser;

import java.security.Principal;

@Slf4j
@RestController("AccountControllerV1")
@Validated
@RequestMapping("api/v1/account")
@RequiredArgsConstructor
public class AccountControllerV1 extends BaseController {

    final AccountServiceImpl service;

    private final String ENTITY_NAME = Account.class.getSimpleName();
    private static final String START_GET_ACCOUNT_FROM_AUTHENTICATED_USER =
            "Start get account from authenticated user: {}";
    private static final String DONE_GET_ACCOUNT_FROM_AUTHENTICATED_USER =
            "Done get account from authenticated user: {}";

    @PostMapping()
    public ResponseEntity<AccountDto> create(
            @Validated(Create.class) @RequestBody AccountCreateDto request) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, request);
        Account createdAccount = service.create(request);
        AccountDto returnedAccount = modelMapper.map(createdAccount, AccountDto.class);
        log.info(CREATE_FINISH_MESSAGE, ENTITY_NAME, createdAccount);
        return new ResponseEntity<>(returnedAccount, HttpStatus.CREATED);
    }

    @PutMapping()
    public ResponseEntity<AccountDto> update(Principal user,
            @Validated(Update.class) @RequestBody AccountDto request) {
        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, request);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
        Account account = modelMapper.map(request, Account.class);
        account.setId(jwtUser.getAccountId());
        Account updatedAccount = service.update(account);
        AccountDto returnedAccount = modelMapper.map(updatedAccount, AccountDto.class);
        log.info(UPDATE_FINISH_MESSAGE, ENTITY_NAME, updatedAccount);
        return new ResponseEntity<>(returnedAccount, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<AccountDto> get(Principal user) {
        log.debug(START_GET_ACCOUNT_FROM_AUTHENTICATED_USER, user);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
        Account foundAccount = service.findById(jwtUser.getAccountId());
        AccountDto returnedAccount = modelMapper.map(foundAccount, AccountDto.class);
        log.debug(DONE_GET_ACCOUNT_FROM_AUTHENTICATED_USER, foundAccount);
        return new ResponseEntity<>(returnedAccount, HttpStatus.OK);
    }
}
