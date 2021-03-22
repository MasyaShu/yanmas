package ru.itterminal.botdesk.files.controller;

import java.io.IOException;
import java.security.Principal;
import java.util.UUID;

import org.springframework.core.io.FileSystemResource;
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
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
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
    public static final String GET_REQUEST_FOR_SAVE_DATA_FOR_FILE_ID = "Get request for save data for fileId: {}";
    public static final String DONE_REQUEST_FOR_SAVE_DATA_FOR_FILE_ID = "Done request for save data for fileId: {}";
    private final String ENTITY_NAME = File.class.getSimpleName();

    private final FileServiceImpl fileService;

    @PostMapping()
    public ResponseEntity<FileDto> create
            (Principal principal,
             @Validated(Create.class) @RequestBody FileDto fileDto) {
        log.debug(CREATE_INIT_MESSAGE, ENTITY_NAME, fileDto);
        var jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        var file = fileService.convertRequestDtoIntoEntityWithNestedObjectsWithOnlyId(fileDto, jwtUser.getAccountId());
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
        var file = fileService.convertRequestDtoIntoEntityWithNestedObjectsWithOnlyId(fileDto, jwtUser.getAccountId());
        var updatedFile = fileService.update(file);
        var returnedFile = modelMapper.map(updatedFile, FileDto.class);
        log.info(UPDATE_FINISH_MESSAGE, ENTITY_NAME, updatedFile);
        return new ResponseEntity<>(returnedFile, HttpStatus.OK);
    }

    @GetMapping("/{fileId}/data")
    public FileSystemResource getFileData(Principal principal, @PathVariable("fileId") UUID fileId) {
        log.debug(GET_REQUEST_FOR_GET_DATA_FOR_FILE_ID, fileId);
        var jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
        return fileService.getFileData(jwtUser.getAccountId(), fileId);
    }

    @PostMapping("/{fileId}/data")
    @ResponseStatus(value = HttpStatus.CREATED)
    public ResponseEntity<Boolean> putFileData(Principal principal,
                                               @PathVariable("fileId") UUID fileId,
                                               @RequestParam("file") MultipartFile file
    ) throws IOException {
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
        fileService.putFileData(jwtUser.getAccountId(), jwtUser.getId(), fileId, bytesOfFile);
        log.debug(DONE_REQUEST_FOR_SAVE_DATA_FOR_FILE_ID, fileId);
        return new ResponseEntity<>(true, HttpStatus.OK);
    }
}
