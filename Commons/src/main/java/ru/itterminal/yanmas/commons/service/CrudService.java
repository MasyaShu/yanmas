package ru.itterminal.yanmas.commons.service;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import ru.itterminal.yanmas.commons.model.BaseEntity;

import java.util.List;
import java.util.UUID;

@SuppressWarnings("unused")
public interface  CrudService<E extends BaseEntity> {

    String CREATE_INIT_MESSAGE = "Dictionary: '%s'. Creating new entity: '%s'";
    String DELETE_INIT_MESSAGE = "Deleting entity: '%s'";
    String FIND_INIT_MESSAGE = "Search for entity with %s: '%s'";
    String ALL_FIND_INIT_MESSAGE = "Search for ALL entities and group it as: '%s'";
    String ALL_FIND_BY_FILTER_INIT_MESSAGE = "Search for ALL entities by filter and group it as: '%s'";
    String UPDATE_INIT_MESSAGE = "Dictionary: '%s'. Updating entity with id: '%s' by following data: '%s' ";

    String CREATE_FINISH_MESSAGE = "Dictionary: '%s'. New entity was created: '%s'";
    String DELETE_FINISH_MESSAGE = "Entity was deleted: '%s'";
    String FIND_FINISH_MESSAGE = "Find by %s: '%s' Entity: '%s' ";
    String ALL_FIND_FINISH_MESSAGE = "Find '%d' entities on page '%d'";
    String ALL_FIND_BY_FILTER_FINISH_MESSAGE = "Find '%d' entities on page '%d'";
    String UPDATE_FINISH_MESSAGE = "Dictionary: '%s'. Entity with id: '%s' now have been updated by new parameters: '%s'";
    String FIND_INVALID_MESSAGE = "Could not find entity by %s: '%s'";
    String VERSION_INVALID_MESSAGE = "Entity with id: '%s' was already modified";
    String ENTITY_NOT_EXIST_MESSAGE = "%s with id:'%s' not exist";

    E create(E entity);

    E findById(UUID id);

    Page<E> findAll(Pageable pageable);

    List<E> findAll();

    Page<E> findAllByFilter(Specification<E> specification, Pageable pageable);

    E update(E entity);
}
