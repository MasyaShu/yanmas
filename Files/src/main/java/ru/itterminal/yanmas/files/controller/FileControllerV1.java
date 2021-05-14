package ru.itterminal.yanmas.files.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.core.io.FileSystemResource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import ru.itterminal.yanmas.aau.controller.BaseControllerImpl;
import ru.itterminal.yanmas.commons.model.dto.BaseFilterDto;
import ru.itterminal.yanmas.commons.model.validator.scenario.Create;
import ru.itterminal.yanmas.commons.model.validator.scenario.Update;
import ru.itterminal.yanmas.files.model.File;
import ru.itterminal.yanmas.files.model.dto.FileDto;
import ru.itterminal.yanmas.files.service.FileServiceImpl;

import java.io.IOException;
import java.util.UUID;

@SuppressWarnings({"DuplicatedCode", "unchecked", "rawtypes"})
@RestController("FileControllerV1")
@Validated
@RequestMapping("api/v1/file")
@RequiredArgsConstructor
public class FileControllerV1 extends BaseControllerImpl<
        File,
        FileServiceImpl,
        FileDto,
        FileDto,
        BaseFilterDto> {

    private final FileServiceImpl fileService;

    private static final Class responseClazz = FileDto.class;
    private static final Class entityClazz = File.class;

    @PostMapping()
    public ResponseEntity<FileDto>
    create(@Validated(Create.class) @RequestBody FileDto request) {
        return create(request, entityClazz, responseClazz);
    }

    @PutMapping()
    public ResponseEntity<FileDto>
    update(@Validated(Update.class) @RequestBody FileDto request) {
        return update(request, entityClazz, responseClazz);
    }

    @GetMapping("/{fileId}/data")
    public FileSystemResource getFileData(@PathVariable("fileId") UUID fileId) {
        var currentUser = getCurrentUser();
        return fileService.getFileData(currentUser, fileId);
    }

    @PostMapping("/{fileId}/data")
    @ResponseStatus(value = HttpStatus.CREATED)
    public ResponseEntity<Boolean> putFileData(@PathVariable("fileId") UUID fileId,
                                               @RequestParam("file") MultipartFile file
    ) throws IOException {
        byte[] bytesOfFile;
        try {
            bytesOfFile = file.getBytes();
        } catch (IOException e) {
            return new ResponseEntity<>(false, HttpStatus.BAD_REQUEST);
        }
        var currentUser = getCurrentUser();
        fileService.putFileData(currentUser, fileId, bytesOfFile);
        return new ResponseEntity<>(true, HttpStatus.OK);
    }
}
