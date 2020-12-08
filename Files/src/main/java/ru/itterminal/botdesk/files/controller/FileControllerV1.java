package ru.itterminal.botdesk.files.controller;

import java.io.IOException;
import java.security.Principal;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

import javax.validation.constraints.NotEmpty;

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
import ru.itterminal.botdesk.files.model.File;
import ru.itterminal.botdesk.files.model.FileDto;
import ru.itterminal.botdesk.files.service.FileServiceImpl;
import ru.itterminal.botdesk.security.jwt.JwtUser;

@Slf4j
@RestController("FileControllerV1")
@Validated
@RequestMapping("api/v1/file")
public class FileControllerV1 extends BaseController {

    private final FileServiceImpl fileService;
    private final AccountServiceImpl accountService;

    public FileControllerV1(FileServiceImpl fileService,
                            AccountServiceImpl accountService) {
        this.fileService = fileService;
        this.accountService = accountService;
    }

    private final String ENTITY_NAME = File.class.getSimpleName();

    @PostMapping()
    @ResponseStatus(value = HttpStatus.CREATED)
    public ResponseEntity<FileDto> create(Principal principal,
                                          @RequestParam("entityId") UUID entityId,
                                          @RequestParam("fileName") @NotEmpty String fileName,
                                          @RequestParam("file") MultipartFile file
    ) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, entityId);
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        byte[] bytesOfFile;
        try {
            bytesOfFile = file.getBytes();
        }
        catch (IOException e) {
            return null;
        }
        File fileEntity = File.builder()
                .account(accountService.findById(jwtUser.getAccountId()))
                .size(bytesOfFile.length)
                .fileName(fileName)
                .entityId(entityId)
                .createdAt(System.currentTimeMillis())
                .build();
        fileEntity.setDeleted(false);
        File createdFile = fileService.create(fileEntity, bytesOfFile);
        FileDto returnedFile = modelMapper.map(createdFile, FileDto.class);
        log.info(CREATE_FINISH_MESSAGE, ENTITY_NAME, createdFile);
        return new ResponseEntity<>(returnedFile, HttpStatus.CREATED);
    }

    @GetMapping("/list")
    public ResponseEntity<List<FileDto>> getAllByEntityIdAndAccountId(
            Principal principal,
            @RequestParam("entityId") UUID entityId
    ) {
        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        Account account = accountService.findById(jwtUser.getAccountId());
        List<File> fileList = fileService.findAllByEntityIdAndAccountId(account.getId(), entityId);
        List<FileDto> fileDtoList = fileList.stream()
                .map(file -> modelMapper.map(file, FileDto.class))
                .collect(Collectors.toList());
        return new ResponseEntity<>(fileDtoList, HttpStatus.OK);
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

}
