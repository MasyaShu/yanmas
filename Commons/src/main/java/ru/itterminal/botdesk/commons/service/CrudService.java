package ru.itterminal.botdesk.commons.service;

import java.util.List;
import java.util.UUID;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;

import ru.itterminal.botdesk.commons.model.BaseEntity;

/**
 * General template for CRUD functional in service layer.
 *
 * @param <E> extends BaseEntity
 */
public interface  CrudService<E extends BaseEntity> {

    String CREATE_INIT_MESSAGE = "Dictionary: '%s'. Creating new entity: '%s'";
    String FIND_INIT_MESSAGE = "Search for entity with %s: '%s'";
    String ALL_FIND_INIT_MESSAGE = "Search for ALL entities and group it as: '%s'";
    String ALL_FIND_BY_FILTER_INIT_MESSAGE = "Search for ALL entities by filter and group it as: '%s'";
    String MARKED_FIND_INIT_MESSAGE = "Search for all entities with delete parameter='%s' " +
        "and group it is: '%s'";
    String UPDATE_INIT_MESSAGE = "Dictionary: '%s'. Updating entity with id: '%s' by following data: '%s' ";
    String LOGICAL_DELETE_INIT_MESSAGE = "Setting delete parameter as '%s' for entity with id: '%s' and version: '%s'";
    String PHYSICAL_DELETE_INIT_MESSAGE = "Removing entity: '%s' with dependent entities";
    String FIND_BY_UNIQUE_FIELDS = "findByUniqueFields({})";
    String FOUND_BY_UNIQUE_FIELDS = "Found by unique fields: {}";

    String CREATE_FINISH_MESSAGE = "Dictionary: '%s'. New entity was created: '%s'";
    String FIND_FINISH_MESSAGE = "Find by %s: '%s' Entity: '%s' ";
    String ALL_FIND_FINISH_MESSAGE = "Find '%d' entities on page '%d'";
    String ALL_FIND_BY_FILTER_FINISH_MESSAGE = "Find '%d' entities on page '%d'";
    String MARKED_FIND_FINISH_MESSAGE = "Find '%d' entities with parameter 'delete=%s' on page '%d'";
    String UPDATE_FINISH_MESSAGE = "Dictionary: '%s'. Entity with id: '%s' now have been updated by new parameters: '%s'";
    String LOGICAL_DELETE_FINISH_MESSAGE = "Successfully changed delete mark to '%s' for entity with id: '%s' and version: '%s'";
    String PHYSICAL_DELETE_FINISH_MESSAGE = "Successfully remove entity with id: '%s' with dependent entities";

    String INVALID_REQUEST = "Trying to get results with null settings";
    String FIND_INVALID_MESSAGE = "Could not find entity by %s: '%s'";
    String VERSION_INVALID_MESSAGE = "Entity with id: '%s' was already modified";
    String ENTITY_NOT_EXIST_MESSAGE = "%s with id:'%s' not exist";
    String CANT_UPDATE_DELETED_MESSAGE = "Dictionary: '%s'. Entity in database is marked as deleted. Can't updated entity '%s'";
    String PASSED_ENTITY_INAPPROPRIATE_MESSAGE = "Dictionary: '%s'. Passed entity is marked as deleted. Can't updated entity '%s'";
    String MISMATCH_DELETE_STATUS_MESSAGE = "Can't set delete parameter = '%s' for entity with id: '%s' and version '%s'";
    String PHYSICAL_DELETE_ERROR_MESSAGE = "Dictionary: '%s'. Can't remove entity '%s' from database. Not all related entities mark for delete.";


    E create(E entity);

    E findById(UUID id);

    Page<E> findAll(Pageable pageable);

    List<E> findAll();

    Page<E> findAllByFilter(Specification<E> specification, Pageable pageable);

    E update(E entity);

    void physicalDelete(UUID id);
}
