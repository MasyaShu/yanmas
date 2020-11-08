package ru.itterminal.botdesk.commons.service.impl;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.data.domain.*;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.exception.NullEntityException;
import ru.itterminal.botdesk.commons.model.GeneralEntity;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;
import ru.itterminal.botdesk.commons.repository.GeneralEntityRepository;
import ru.itterminal.botdesk.commons.service.CrudService;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;

import javax.persistence.OptimisticLockException;
import java.time.ZonedDateTime;
import java.util.Collections;
import java.util.NoSuchElementException;
import java.util.Optional;
import java.util.UUID;

import static java.lang.String.format;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.times;

@ExtendWith(SpringExtension.class)
@ContextConfiguration(classes = {BasicOperationValidatorImpl.class, GeneralEntityService.class, BaseEntitySpec.class})
class CrudServiceImplTest {

    @MockBean
    private GeneralEntityRepository repository;

    @MockBean
    private BasicOperationValidatorImpl<GeneralEntity> validator;

    @Autowired
    private GeneralEntityService service;

    @Autowired
    private BaseEntitySpec spec;

    private GeneralEntity testEntity;
    private GeneralEntity baseEntity;
    private Page<GeneralEntity> expectedPage;

    private final Pageable pageable = PageRequest.of(WORK_PAGE, WORK_PAGE_SIZE, Sort.by(SORT_CRITERIA));
    private static final int WORK_PAGE = 0;
    private static final int WORK_PAGE_SIZE = 5;
    private static final String SORT_CRITERIA = "name";
    private static final String TEST_ENTITY_NAME = "general-test-entity-00";
    private static final String TEST_ENTITY_DESCRIPTION = "general-test-description-00";
    private static final UUID TEST_ENTITY_ID = UUID.randomUUID();
    private static final String VERSION_INVALID_MESSAGE = "Entity with id: '%s' was already modified";

    @BeforeEach
    void initTestEnvironment() {
        testEntity = GeneralEntity.builder()
                .date(ZonedDateTime.now())
                .name(TEST_ENTITY_NAME)
                .description(TEST_ENTITY_DESCRIPTION)
                .build();
        baseEntity = GeneralEntity.builder()
                .date(testEntity.getDate())
                .name(testEntity.getName())
                .description(testEntity.getDescription())
                .build();
        baseEntity.setDeleted(false);
        baseEntity.setId(TEST_ENTITY_ID);
        expectedPage = new PageImpl<>(Collections.singletonList(baseEntity));
        Mockito.when(validator.beforeCreate(null)).thenThrow(NullEntityException.class);
        Mockito.when(validator.beforeUpdate(null)).thenThrow(NullEntityException.class);
        Mockito.when(validator.checkLogicalDelete(TEST_ENTITY_ID)).thenReturn(true);
    }

    @Test
    void create_shouldCreateEntity_whenNameOrIdIsUnique() {
        Mockito.when(repository.create(testEntity)).thenReturn(baseEntity);
        GeneralEntity actualEntity = service.create(testEntity);
        assertEquals(TEST_ENTITY_ID, actualEntity.getId());
    }

    @Test
    void create_shouldThrowDataIntegrityViolationException_whenNameOrIdNotUnique() {
        Mockito.when(repository.create(testEntity)).thenThrow(DataIntegrityViolationException.class);
        assertThrows(DataIntegrityViolationException.class, () -> service.create(testEntity));
    }

    @Test
    void create_shouldThrowNullEntityException_whenEntityNull() {
        assertThrows(NullEntityException.class, () -> service.create(null));
    }

    @Test
    void findById_shouldReturnEntity_whenEntityExist() {
        Mockito.when(repository.existsById(TEST_ENTITY_ID)).thenReturn(true);
        Mockito.when(repository.findById(TEST_ENTITY_ID)).thenReturn(Optional.of(baseEntity));
        GeneralEntity actualEntity = service.findById(TEST_ENTITY_ID);
        assertEquals(baseEntity, actualEntity);
    }

    @Test
    void findById_shouldThrowEntityNotExistException_whenEntityNotExist() {
        Mockito.when(repository.findById(TEST_ENTITY_ID)).thenReturn(Optional.empty());
        assertThrows(EntityNotExistException.class, () -> service.findById(TEST_ENTITY_ID));
    }

    @Test
    void findById_shouldThrowExceptionSameAsRepository_whenRepositoryThrowException() {
        Mockito.when(repository.existsById(TEST_ENTITY_ID)).thenReturn(true);
        Mockito.when(repository.findById(TEST_ENTITY_ID)).thenThrow(NoSuchElementException.class);
        assertThrows(NoSuchElementException.class, () -> service.findById(TEST_ENTITY_ID));
    }

    @Test
    void findAllByFilter_shouldReturnPageOfGeneralEntity_whenEntitiesExistByFilter() {
        Specification<GeneralEntity> generalEntitySpecification = spec.getEntityByDeletedSpec(BaseFilterDto.FilterByDeleted.ALL);
        Mockito.when(repository.findAll(generalEntitySpecification, pageable)).thenReturn(expectedPage);
        Page<GeneralEntity> actualPage = service.findAllByFilter(generalEntitySpecification, pageable);
        assertEquals(expectedPage, actualPage);
    }

    @Test
    void findAllByFilter_shouldReturnEmptyPage_whenEntitiesNotExistByFilter() {
        Specification<GeneralEntity> generalEntitySpecification = spec.getEntityByDeletedSpec(BaseFilterDto.FilterByDeleted.ALL);
        Mockito.when(repository.findAll(generalEntitySpecification, pageable)).thenReturn(Page.empty());
        Page<GeneralEntity> actualPage = service.findAllByFilter(generalEntitySpecification, pageable);
        assertEquals(Page.empty(), actualPage);
    }

    @Test
    void findAll_shouldReturnPageOfGeneralEntity_whenEntitiesExist() {
        Mockito.when(repository.findAll(pageable)).thenReturn(expectedPage);
        Page<GeneralEntity> actualPage = service.findAll(pageable);
        assertEquals(expectedPage, actualPage);
    }

    @Test
    void findAll_shouldReturnEmptyPage_whenEntitiesNotExist() {
        Mockito.when(repository.findAll(pageable)).thenReturn(Page.empty());
        Page<GeneralEntity> actualPage = service.findAll(pageable);
        assertEquals(Page.empty(), actualPage);
    }

    @Test
    void update_shouldReturnUpdatedEntity_whenEntityExistAndNotMarkedAsDeleted() {
        testEntity.setDeleted(false);
        Mockito.when(repository.update(testEntity)).thenReturn(baseEntity);
        Mockito.when(repository.existsById(testEntity.getId())).thenReturn(true);
        GeneralEntity actualEntity = service.update(testEntity);
        assertEquals(baseEntity, actualEntity);
    }

    @Test
    void update_shouldThrowEntityNotExistException_whenEntityNotExistInDatabase() {
        Mockito.when(repository.findById(baseEntity.getId())).thenReturn(Optional.empty());
        Throwable thrown = assertThrows(EntityNotExistException.class, () -> service.update(baseEntity));
        String actual = thrown.getMessage();
        String expected = format(CrudService.ENTITY_NOT_EXIST_MESSAGE,
                baseEntity.getClass().getSimpleName(), baseEntity.getId());
        assertEquals(expected, actual);
        Mockito.verify(repository, times(0)).update(baseEntity);
    }

    @Test
    void update_shouldThrowExceptionSameAsRepository_whenRepositoryThrowException() {
        testEntity.setDeleted(false);
        Mockito.when(repository.update(testEntity)).thenThrow(DataIntegrityViolationException.class);
        Mockito.when(repository.existsById(testEntity.getId())).thenReturn(true);
        assertThrows(DataIntegrityViolationException.class, () -> service.update(testEntity));
    }

    @Test
    void update_shouldThrowOptimisticLockingFailureException_whenRepoThrowsObjectOptimisticLockingFailureException() {
        testEntity.setId(TEST_ENTITY_ID);
        testEntity.setDeleted(false);
        Mockito.when(repository.update(testEntity)).thenThrow(OptimisticLockException.class);
        Mockito.when(repository.existsById(testEntity.getId())).thenReturn(true);
        Throwable thrown = assertThrows(OptimisticLockingFailureException.class, () -> service.update(testEntity));
        String expected = format(VERSION_INVALID_MESSAGE, testEntity.getId());
        assertEquals(expected, thrown.getMessage());
    }

    @Test
    void update_shouldThrowNullEntityException_whenEntityNull() {
        assertThrows(NullEntityException.class, () -> service.update(null));
    }

}
