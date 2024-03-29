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
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.commons.model.validator.scenario.Create;
import ru.itterminal.botdesk.commons.model.validator.scenario.Update;
import ru.itterminal.botdesk.files.model.File;
import ru.itterminal.botdesk.files.model.dto.FileDto;
import ru.itterminal.botdesk.files.service.FileServiceImpl;
import ru.itterminal.botdesk.security.jwt.JwtUser;

@Slf4j
@RestController("FileControllerV1")
@Validated
@RequestMapping("api/v1/file")
@RequiredArgsConstructor
public class FileControllerV1 extends BaseController {

    public static final String GET_REQUEST_FOR_GET_DATA_FOR_FILE_ID = "Get request for get data for fileId: {}";
    public static final String DONE_REQUEST_FOR_GET_DATA_FOR_FILE_ID_SIZE_OF_DATA_IS =
            "Done request for get data for fileId: {}, size of data is {}";
    public static final String GET_REQUEST_FOR_SAVE_DATA_FOR_FILE_ID = "Get request for save data for fileId: {}";
    public static final String DONE_REQUEST_FOR_SAVE_DATA_FOR_FILE_ID = "Done request for save data for fileId: {}";

    private final FileServiceImpl fileService;
    private final AccountServiceImpl accountService;

    private final String ENTITY_NAME = File.class.getSimpleName();

    @PostMapping()
    public ResponseEntity<FileDto> create
            (Principal principal,
             @Validated(Create.class) @RequestBody FileDto fileDto) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, fileDto);
        var jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        var file = modelMapper.map(fileDto, File.class);
        setNestedObjectsIntoEntityFromEntityDtoRequest(file, jwtUser.getAccountId());
        var createdFile = fileService.create(file);
        var returnedFile = modelMapper.map(createdFile, FileDto.class);
        log.info(CREATE_FINISH_MESSAGE, ENTITY_NAME, createdFile);
        return new ResponseEntity<>(returnedFile, HttpStatus.CREATED);
    }

    @PutMapping()
    public ResponseEntity<FileDto> update
            (Principal principal,
             @Validated(Update.class) @RequestBody FileDto fileDto) {
        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, fileDto);
        var jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        var file = modelMapper.map(fileDto, File.class);
        setNestedObjectsIntoEntityFromEntityDtoRequest(file, jwtUser.getAccountId());
        var updatedFile = fileService.update(file);
        var returnedFile = modelMapper.map(updatedFile, FileDto.class);
        log.info(UPDATE_FINISH_MESSAGE, ENTITY_NAME, updatedFile);
        return new ResponseEntity<>(returnedFile, HttpStatus.OK);
    }

    @GetMapping("/{fileId}/data")
    public ResponseEntity<Resource> getFileData(
            Principal principal,
            @PathVariable("fileId") UUID fileId
    ) {
        log.debug(GET_REQUEST_FOR_GET_DATA_FOR_FILE_ID, fileId);
        var jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        byte[] fileData = fileService.getFileData(jwtUser.getAccountId(), fileId);
        ByteArrayResource resource = new ByteArrayResource(fileData);
        log.debug(DONE_REQUEST_FOR_GET_DATA_FOR_FILE_ID_SIZE_OF_DATA_IS, fileId, fileData.length);
        return ResponseEntity.ok()
                .contentLength(fileData.length)
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .body(resource);

    }

    @PostMapping("/{fileId}/data")
    @ResponseStatus(value = HttpStatus.CREATED)
    public ResponseEntity<Boolean> putFileData(Principal principal,
                                               @PathVariable("fileId") UUID fileId,
                                               @RequestParam("file") MultipartFile file
    ) {
        log.debug(GET_REQUEST_FOR_SAVE_DATA_FOR_FILE_ID, fileId);
        var jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        byte[] bytesOfFile;
        try {
            bytesOfFile = file.getBytes();
        }
        catch (IOException e) {
            log.error(e.getMessage());
            return new ResponseEntity<>(false, HttpStatus.BAD_REQUEST);
        }
        var result = fileService.putFileData(jwtUser.getAccountId(), jwtUser.getId(), fileId, bytesOfFile);
        log.debug(DONE_REQUEST_FOR_SAVE_DATA_FOR_FILE_ID, fileId);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    private void setNestedObjectsIntoEntityFromEntityDtoRequest(File file, UUID accountId) {
        file.setAccount(accountService.findById(accountId));
    }

}
