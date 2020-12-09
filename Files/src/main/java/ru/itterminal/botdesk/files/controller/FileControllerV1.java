package ru.itterminal.botdesk.files.controller;

import java.io.IOException;
import java.security.Principal;
import java.util.UUID;

import org.springframework.core.io.ByteArrayResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.files.service.FileServiceImpl;
import ru.itterminal.botdesk.security.jwt.JwtUser;

@Slf4j
@RestController("FileControllerV1")
@Validated
@RequestMapping("api/v1/file-data")
public class FileControllerV1 extends BaseController {

    private final FileServiceImpl fileService;
    private final AccountServiceImpl accountService;

    public FileControllerV1(FileServiceImpl fileService,
                            AccountServiceImpl accountService) {
        this.fileService = fileService;
        this.accountService = accountService;
    }

    @GetMapping()
    public ResponseEntity<Resource> getFileData(
            Principal principal,
            @RequestParam("fileId") UUID fileId
    ) {
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        Account account = accountService.findById(jwtUser.getAccountId());
        byte[] fileData = fileService.getFileData(account.getId(), fileId);
        ByteArrayResource resource = new ByteArrayResource(fileData);
        return ResponseEntity.ok()
                .contentLength(fileData.length)
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .body(resource);
    }

    @PostMapping()
    @ResponseStatus(value = HttpStatus.CREATED)
    public ResponseEntity<Boolean> putFileData(Principal principal,
                                               @RequestParam("fileId") UUID fileId,
                                               @RequestParam("file") MultipartFile file
    ) {
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        Account account = accountService.findById(jwtUser.getAccountId());
        byte[] bytesOfFile;
        try {
            bytesOfFile = file.getBytes();
        }
        catch (IOException e) {
            log.error(e.getMessage());
            return new ResponseEntity<>(false, HttpStatus.BAD_REQUEST);
        }
        boolean result = fileService.putFileData(account.getId(), fileId, bytesOfFile);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

}
