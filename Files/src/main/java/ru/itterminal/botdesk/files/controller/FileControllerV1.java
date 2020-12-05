package ru.itterminal.botdesk.files.controller;

import java.io.IOException;
import java.security.Principal;
import java.util.UUID;

import javax.validation.constraints.NotEmpty;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import lombok.extern.slf4j.Slf4j;
import ru.itterminal.botdesk.aau.service.impl.AccountServiceImpl;
import ru.itterminal.botdesk.commons.controller.BaseController;
import ru.itterminal.botdesk.files.model.File;
import ru.itterminal.botdesk.files.model.FileDto;
import ru.itterminal.botdesk.files.service.FileServiceImpl;
import ru.itterminal.botdesk.security.jwt.JwtUser;

@SuppressWarnings("DuplicatedCode")
@Slf4j
@RestController("FileControllerV1")
@Validated
@RequestMapping("api/v1/file")
public class FileControllerV1 extends BaseController {

    final FileServiceImpl fileService;
    final AccountServiceImpl accountService;

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

    //    @PostMapping("/check-access")
    //    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR')")
    //    public ResponseEntity<String> createCheckAccess() {
    //        String message = format(SUCCESSFUL_CHECK_ACCESS, WORD_CREATE, ENTITY_NAME);
    //        log.trace(message);
    //        return ResponseEntity.ok(message);
    //    }
    //
    //    @PutMapping()
    //    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR')")
    //    public ResponseEntity<UserDtoResponseWithoutPassword> update( Principal principal,
    //            @Validated(Update.class) @RequestBody UserDto request) {
    //        log.debug(UPDATE_INIT_MESSAGE, ENTITY_NAME, request);
    //        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) principal).getPrincipal());
    //        User user = modelMapper.map(request, User.class);
    //        user.setAccount(accountService.findById(jwtUser.getAccountId()));
    //        user.setRole(roleService.findById(request.getRoleId()));
    //        user.setOwnGroup(groupService.findById(request.getGroupId()));
    //        User updatedUser = userService.update(user);
    //        UserDtoResponseWithoutPassword returnedUser =
    //                modelMapper.map(updatedUser, UserDtoResponseWithoutPassword.class);
    //        log.info(UPDATE_FINISH_MESSAGE, ENTITY_NAME, updatedUser);
    //        return new ResponseEntity<>(returnedUser, HttpStatus.OK);
    //    }
    //
    //    @PutMapping("/check-access")
    //    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN', 'EXECUTOR')")
    //    public ResponseEntity<String> updateCheckAccess() {
    //        String message = format(SUCCESSFUL_CHECK_ACCESS, WORD_UPDATE, ENTITY_NAME);
    //        log.trace(message);
    //        return ResponseEntity.ok(message);
    //    }
    //
    //    @GetMapping("/{id}")
    //    public ResponseEntity<UserDtoResponseWithoutPassword> getById(Principal user, @PathVariable UUID id) {
    //        log.debug(FIND_BY_ID_INIT_MESSAGE, ENTITY_NAME, id);
    //        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
    //        User foundUser;
    //        if (jwtUser.isInnerGroup()) {
    //            foundUser = userService.findByIdAndAccountId(id, jwtUser.getAccountId());
    //        } else {
    //            foundUser = userService.findByIdAndAccountIdAndOwnGroupId(id, jwtUser.getAccountId(), jwtUser.getGroupId());
    //        }
    //        UserDtoResponseWithoutPassword returnedUser = modelMapper.map(foundUser, UserDtoResponseWithoutPassword.class);
    //        log.debug(FIND_BY_ID_FINISH_MESSAGE, ENTITY_NAME, foundUser);
    //        return new ResponseEntity<>(returnedUser, HttpStatus.OK);
    //    }
    //
    //    @SuppressWarnings("ConstantConditions")
    //    @GetMapping()
    //    public ResponseEntity<Page<UserDtoResponseWithoutPassword>> getByFilter(
    //            Principal user,
    //            @Valid @RequestBody UserFilterDto filter,
    //            @RequestParam(defaultValue = PAGE_DEFAULT_VALUE) @PositiveOrZero int page,
    //            @RequestParam(defaultValue = SIZE_DEFAULT_VALUE) @Positive int size) {
    //        log.debug(FIND_INIT_MESSAGE, ENTITY_NAME, page, size, filter);
    //        JwtUser jwtUser = ((JwtUser) ((UsernamePasswordAuthenticationToken) user).getPrincipal());
    //        if (filter.getDirection() == null) {
    //            filter.setDirection("ASC");
    //        }
    //        if (filter.getSortBy() == null) {
    //            filter.setSortBy("firstName");
    //        }
    //        if (filter.getDeleted() == null) {
    //            filter.setDeleted("all");
    //        }
    //        Pageable pageable =
    //                PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(filter.getDirection()),
    //                        filter.getSortBy()));
    //        Page<User> foundUsers;
    //        Page<UserDtoResponseWithoutPassword> returnedUsers;
    //        Specification<User> userSpecification = Specification
    //                .where(filter.getEmail() == null ? null : spec.getUserByEmailSpec(filter.getEmail()))
    //                .and(filter.getFirstName() == null ? null : spec.getUserByFirstNameSpec(filter.getFirstName()))
    //                .and(filter.getSecondName() == null ? null : spec.getUserBySecondNameSpec(filter.getSecondName()))
    //                .and(filter.getPhone() == null ? null : spec.getUserByPhoneSpec(filter.getPhone()))
    //                .and(filter.getComment() == null ? null : spec.getUserByCommentSpec(filter.getComment()))
    //                .and(filter.getIsArchived() == null ? null : spec.getUserByIsArchivedSpec(filter.getIsArchived()))
    //                .and(filter.getGroups() == null || filter.getGroups().isEmpty() ? null :
    //                        spec.getUserByListOfGroupsSpec(filter.getGroups()))
    //                .and(jwtUser.isInnerGroup() ? null :
    //                        spec.getUserByListOfGroupsSpec(List.of(jwtUser.getGroupId())))
    //                .and(filter.getRoles() == null || filter.getRoles().isEmpty() ? null :
    //                        spec.getUserByListOfRolesSpec(filter.getRoles()))
    //                .and(spec.getEntityByDeletedSpec(BaseFilterDto.FilterByDeleted.fromString(filter.getDeleted())))
    //                .and(spec.getEntityByAccountSpec(jwtUser.getAccountId()))
    //                .and(filter.getOutId() == null ? null :  spec.getEntityByOutIdSpec(filter.getOutId()));
    //        foundUsers = userService.findAllByFilter(userSpecification, pageable);
    //        returnedUsers = mapPage(foundUsers, UserDtoResponseWithoutPassword.class, pageable);
    //        log.debug(FIND_FINISH_MESSAGE, ENTITY_NAME, foundUsers.getTotalElements());
    //        return new ResponseEntity<>(returnedUsers, HttpStatus.OK);
    //    }
    //
    //    @DeleteMapping()
    //    @PreAuthorize("hasAnyAuthority('ACCOUNT_OWNER', 'ADMIN')")
    //    public ResponseEntity<Void> physicalDelete() {
    //        throw new UnsupportedOperationException("Physical delete will be implement in the further");
    //    }
}
