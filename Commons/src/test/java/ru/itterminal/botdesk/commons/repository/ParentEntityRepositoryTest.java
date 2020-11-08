package ru.itterminal.botdesk.commons.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

import java.time.ZonedDateTime;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.dao.InvalidDataAccessApiUsageException;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.exception.NullEntityException;
import ru.itterminal.botdesk.commons.model.GeneralEntity;

/**
 * Test for general methods for each repository.
 * Based on h2 database with postgres dialect and typical, but not existent in project, entity
 *
 */
@SuppressWarnings("deprecation")
@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = RepositoryTestConfig.class)
@Sql({"/create-general-test.sql"})
class ParentEntityRepositoryTest {

    @Autowired
    private GeneralEntityRepository repository;

    private GeneralEntity uniqueGeneralEntity;

    private GeneralEntity notUniqueIdGeneralEntity;
    private GeneralEntity notUniqueNameGeneralEntity;
    private GeneralEntity optimisticLockEntity;


    private final Pageable pageable = PageRequest.of(WORK_PAGE, WORK_PAGE_SIZE, Sort.by(SORT_CRITERIA));

    private static final int WORK_PAGE = 0;
    private static final int WORK_PAGE_SIZE = 5;
    private static final String SORT_CRITERIA = "name";
    private static final String OUT_ID_1 = "OUT_ID_1";
    private static final String OUT_ID_2 = "OUT_ID_2";
    private static final UUID UNIQUE_ID = UUID.fromString("cb9e8816-7bed-4bb6-b3ea-3aa0eee247b6");
    private static final UUID NOT_UNIQUE_ID = UUID.fromString("d592facb-e6ee-4801-8310-9c7708eb6e6c");
    private static final UUID NOT_UNIQUE_ID_MARKED_DELETED = UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de");
    private static final UUID LOCKED_ID = UUID.fromString("62d6a9eb-b902-4c5c-be58-57d9848d3a12");
    private static final String UNIQUE_NAME = "unique name";
    private static final String NOT_UNIQUE_NAME = "not_unique";
    private static final String GENERAL_DESCRIPTION = "general description";

    @SuppressWarnings("unused")
    @BeforeAll
    private void initTestEnvironment() {
        uniqueGeneralEntity = GeneralEntity.builder()
            .name(UNIQUE_NAME)
            .date(ZonedDateTime.now())
            .description(GENERAL_DESCRIPTION)
            .build();
        uniqueGeneralEntity.setId(UNIQUE_ID);
        uniqueGeneralEntity.setOutId(OUT_ID_1);
        uniqueGeneralEntity.setDeleted(false);
        notUniqueIdGeneralEntity = GeneralEntity.builder()
            .name(UNIQUE_NAME)
            .date(ZonedDateTime.now())
            .description(GENERAL_DESCRIPTION)
            .build();
        notUniqueIdGeneralEntity.setId(NOT_UNIQUE_ID);
        notUniqueIdGeneralEntity.setVersion(0);
        notUniqueIdGeneralEntity.setOutId(OUT_ID_2);
        notUniqueIdGeneralEntity.setDeleted(false);
        notUniqueNameGeneralEntity = GeneralEntity.builder()
            .name(NOT_UNIQUE_NAME)
            .date(ZonedDateTime.now())
            .description(GENERAL_DESCRIPTION)
            .build();
        notUniqueNameGeneralEntity.setId(UNIQUE_ID);
        optimisticLockEntity = GeneralEntity.builder()
            .name(UNIQUE_NAME)
            .date(ZonedDateTime.now())
            .description(GENERAL_DESCRIPTION)
            .build();
        optimisticLockEntity.setId(LOCKED_ID);
    }

    @Test
    void create_shouldCreateNewEntity_whenIdAndNameUnique() {
        GeneralEntity createdEntity = repository.create(uniqueGeneralEntity);
        GeneralEntity newBaseEntity = repository.findById(uniqueGeneralEntity.getId()).get();
        long baseSize = StreamSupport.stream(repository.findAll().spliterator(), false).count();
        assertTrue(newBaseEntity.getId().equals(UNIQUE_ID) && baseSize == 15);
        assertEquals(uniqueGeneralEntity, createdEntity);
    }

    @Test
    void create_shouldThrowDataIntegrityViolationException_whenIdNotUnique() {
        assertThrows(DataIntegrityViolationException.class, () -> repository.create(notUniqueIdGeneralEntity));
    }

    @Test
    void create_shouldThrowDataIntegrityViolationException_whenOutIdMoreThatMax() {
        notUniqueIdGeneralEntity.setOutId("1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890"
                + "1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890");
        assertThrows(DataIntegrityViolationException.class, () -> repository.create(notUniqueIdGeneralEntity));
        notUniqueIdGeneralEntity.setOutId(null);
    }

    @Test
    void create_shouldThrowDataIntegrityViolationException_whenNameNotUnique() {
        assertThrows(DataIntegrityViolationException.class, () -> repository.create(notUniqueNameGeneralEntity));
    }

    @Test
    void create_shouldThrowInvalidDataAccessApiUsageException_whenCreateFromNull() {
        assertThrows(InvalidDataAccessApiUsageException.class, () -> repository.create(null));
    }

    @Test
    void update_shouldUpdateAndReturnNewEntityWithIncreasedVersion_whenEntityExistRequiredFieldsUnique() {
        Map<UUID, String> baseEntitiesNameBeforeUpdate = getAllEntitiesName();
        String oldEntityName = repository.findById(notUniqueIdGeneralEntity.getId()).get().getName();
        GeneralEntity newEntity = repository.update(notUniqueIdGeneralEntity);
        Map<UUID, String> baseEntitiesNameAfterUpdate = getAllEntitiesName();
        assertTrue(baseEntitiesNameBeforeUpdate.containsKey(notUniqueIdGeneralEntity.getId()) &&
            baseEntitiesNameBeforeUpdate.containsValue(oldEntityName) &&
            baseEntitiesNameAfterUpdate.containsKey(notUniqueIdGeneralEntity.getId()) &&
            baseEntitiesNameAfterUpdate.containsValue(newEntity.getName()) &&
            !baseEntitiesNameAfterUpdate.containsValue(oldEntityName) &&
            notUniqueIdGeneralEntity.getVersion() + 1 == newEntity.getVersion());
    }

    @Test
    void update_shouldThrowObjectOptimisticLockingFailureException_whenTransactionWasUpdatedInProcess() {
        assertThrows(ObjectOptimisticLockingFailureException.class, () -> repository.update(optimisticLockEntity));
    }

    @Test
    void update_shouldThrowDataIntegrityViolationException_whenEntityExistRequiredFieldsNotUnique() {
        notUniqueNameGeneralEntity.setId(NOT_UNIQUE_ID);
        assertThrows(DataIntegrityViolationException.class, () -> repository.update(notUniqueNameGeneralEntity));
    }

    @Test
    void update_shouldThrowEntityNotExistException_whenEntityNotExist() {
        assertThrows(EntityNotExistException.class, () -> repository.update(uniqueGeneralEntity));
    }

    @Test
    void update_shouldThrowNullEntityException_whenNull() {
        assertThrows(NullEntityException.class, () -> repository.update(null));
    }

    @Test
    void findMarked_shouldGetTwoEntities_whenPassedFalse() {
        Page<GeneralEntity> documentTypes = repository.findMarked(pageable, false);
        assertEquals(2, documentTypes.getTotalElements());
    }

    @Test
    void findMarked_shouldGetTwelveEntities_whenPassedTrue() {
        Page<GeneralEntity> documentTypes = repository.findMarked(pageable, true);
        assertEquals(12, documentTypes.getTotalElements());
    }

    @Test
    void findMarked_shouldAddEntitiesToSinglePage_whenPageableNull() {
        Page<GeneralEntity> documentTypes = repository.findMarked(null, true);
        assertEquals(12, documentTypes.getTotalElements());
    }

    @Test
    void logicalDelete_shouldMarkAsDeletedAndIncreaseVersion_whenTrueFlagAndCorrectIdAndVersionPassed() {
        assertEquals(1, repository.logicalDelete(NOT_UNIQUE_ID, true, 0));

        GeneralEntity actual = repository.findById(NOT_UNIQUE_ID).get();
        assertTrue(actual.getDeleted());
        assertEquals(NOT_UNIQUE_ID, actual.getId());
        assertEquals(Integer.valueOf(1), actual.getVersion());
    }

    @Test
    void logicalDelete_shouldUnmarkAsDeletedAndIncreaseVersion_whenFalseFlagAndCorrectIdAndVersionPassed() {
        assertEquals(1, repository.logicalDelete(NOT_UNIQUE_ID_MARKED_DELETED, false, 0));

        GeneralEntity actual = repository.findById(NOT_UNIQUE_ID_MARKED_DELETED).get();
        assertFalse(actual.getDeleted());
        assertEquals(NOT_UNIQUE_ID_MARKED_DELETED, actual.getId());
        assertEquals(Integer.valueOf(1), actual.getVersion());
    }

    @Test
    void logicalDelete_shouldNotDoAnythingAndReturnZero_whenPassedVersionNotMatchDb() {
        assertEquals(0, repository.logicalDelete(LOCKED_ID, true, 0));

        GeneralEntity actual = repository.findById(LOCKED_ID).get();
        assertFalse(actual.getDeleted());
        assertEquals(LOCKED_ID, actual.getId());
        assertEquals(Integer.valueOf(1), actual.getVersion());
    }

    @Test
    void logicalDelete_shouldNotDoAnythingAndReturnZero_whenPassedIdNotExist() {
        assertEquals(0, repository.logicalDelete(UNIQUE_ID, true, 0));

        assertFalse(repository.findById(UNIQUE_ID).isPresent());
    }

    @Test
    void logicalDelete_shouldReturnZero_whenPassedNullId() {
        assertEquals(0, repository.logicalDelete(null, true, 0));
    }

    private Map<UUID, String> getAllEntitiesName() {
        Iterable<GeneralEntity> generalEntities = repository.findAll();
        return StreamSupport.stream(generalEntities.spliterator(), true)
            .collect(Collectors.toMap(GeneralEntity::getId, GeneralEntity::getName));
    }
}
