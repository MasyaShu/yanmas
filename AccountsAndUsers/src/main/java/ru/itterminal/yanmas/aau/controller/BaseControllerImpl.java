package ru.itterminal.yanmas.aau.controller;

import java.util.UUID;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.yanmas.aau.service.impl.CrudServiceWithAccountImpl;
import ru.itterminal.yanmas.aau.util.ReflectionHelper;
import ru.itterminal.yanmas.commons.controller.BaseController;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.commons.model.dto.BaseEntityDto;
import ru.itterminal.yanmas.commons.model.dto.BaseFilterDto;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;

@SuppressWarnings({"unchecked", "rawtypes", "SpringJavaAutowiredFieldsWarningInspection"})
@Slf4j
@Component
public abstract class BaseControllerImpl<
        Entity extends BaseEntity,
        Service extends CrudServiceWithAccountImpl,
        Request extends BaseEntityDto,
        Response extends BaseEntityDto,
        Filter extends BaseFilterDto>
        extends BaseController {

    @SuppressWarnings("SpringJavaInjectionPointsAutowiringInspection")
    @Autowired
    protected Service service;

    @Autowired
    protected JwtUserBuilder jwtUserBuilder;

    @Autowired
    protected SpecificationsFactory specFactory;

    @Autowired
    ReflectionHelper reflectionHelper;

    @SneakyThrows
    protected ResponseEntity<Response> baseCreate(Request request, Class<Entity> entityClass,
                                                  Class<Response> responseClass) {
        log.debug(CREATE_INIT_MESSAGE, entityClass.getSimpleName(), request);
        var createdEntity = service.create(
                reflectionHelper.convertRequestDtoIntoEntityWhereNestedObjectsWithOnlyValidId(
                        request,
                        entityClass
                )
        );
        var returnedEntity = modelMapper.map(createdEntity, responseClass);
        log.info(CREATE_FINISH_MESSAGE, entityClass.getSimpleName(), createdEntity);
        return new ResponseEntity<>(returnedEntity, HttpStatus.CREATED);
    }

    protected ResponseEntity<Response> baseUpdate(Request request, Class<Entity> entityClass,
                                                  Class<Response> responseClass) {
        log.debug(UPDATE_INIT_MESSAGE, entityClass.getSimpleName(), request);
        var updatedEntity = service.update(
                reflectionHelper.convertRequestDtoIntoEntityWhereNestedObjectsWithOnlyValidId(
                        request,
                        entityClass
                )
        );
        var returnedEntity = modelMapper.map(updatedEntity, responseClass);
        log.info(UPDATE_FINISH_MESSAGE, entityClass.getSimpleName(), updatedEntity);
        return new ResponseEntity<>(returnedEntity, HttpStatus.OK);
    }

    protected ResponseEntity<Response> baseGetById(UUID id, Class<Entity> entityClass, Class<Response> responseClass) {
        log.debug(FIND_BY_ID_INIT_MESSAGE, entityClass.getSimpleName(), id);
        var foundEntity = service.findByIdAndAccountId(id);
        var returnedEntity = modelMapper.map(foundEntity, responseClass);
        log.debug(FIND_BY_ID_FINISH_MESSAGE, entityClass.getSimpleName(), foundEntity);
        return new ResponseEntity<>(returnedEntity, HttpStatus.OK);
    }

    protected ResponseEntity<Page<Response>> baseGetByFilter(Filter filterDto,
                                                             int page,
                                                             int size,
                                                             Class<Entity> entityClass,
                                                             Class<Response> responseClass) {
        log.debug(FIND_INIT_MESSAGE, entityClass.getSimpleName(), page, size, filterDto);
        var pageable = createPageable(size, page, filterDto.getSortByFields(), filterDto.getSortDirection());
        var jwtUser = jwtUserBuilder.getJwtUser();
        var accountId = jwtUser.getAccountId();
        var specification =
                specFactory.makeSpecificationFromEntityFilterDto(entityClass, filterDto, accountId);
        var foundEntities = service.findAllByFilter(specification, pageable);
        var returnedEntities = mapPage(foundEntities, responseClass, pageable);
        log.debug(FIND_FINISH_MESSAGE, entityClass.getSimpleName(), foundEntities.getTotalElements());
        return new ResponseEntity<>(returnedEntities, HttpStatus.OK);
    }

}