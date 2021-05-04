package ru.itterminal.yanmas.aau.controller;

import lombok.SneakyThrows;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.aau.service.business_handler.impl.CrudServiceWithBusinessHandlerImpl;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.aau.util.ReflectionHelper;
import ru.itterminal.yanmas.commons.controller.BaseController;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.commons.model.dto.BaseEntityDto;
import ru.itterminal.yanmas.commons.model.dto.BaseFilterDto;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;

import java.util.UUID;

@SuppressWarnings({"unchecked", "rawtypes", "SpringJavaAutowiredFieldsWarningInspection"})
@Component
public abstract class BaseControllerImpl<
        Entity extends BaseEntity,
        BusinessHandler extends EntityBusinessHandler,
        Service extends CrudServiceWithBusinessHandlerImpl,
        Request extends BaseEntityDto,
        Response extends BaseEntityDto,
        Filter extends BaseFilterDto>
        extends BaseController {

    @Autowired
    protected BusinessHandler businessHandler;

    @Autowired
    protected Service service;

    @Autowired
    protected JwtUserBuilder jwtUserBuilder;

    @Autowired
    protected UserServiceImpl userService;

    @Autowired
    protected SpecificationsFactory specFactory;

    @Autowired
    ReflectionHelper reflectionHelper;

    @SneakyThrows
    protected ResponseEntity<Response> baseCreate(Request request, Class<Entity> entityClass,
                                                  Class<Response> responseClass) {
        var currentUser = getCurrentUser();
        var createdEntity = service.create(
                reflectionHelper.convertRequestDtoIntoEntityWhereNestedObjectsWithOnlyValidId(
                        request,
                        entityClass
                ),
                currentUser
        );
        var returnedEntity = modelMapper.map(createdEntity, responseClass);
        return new ResponseEntity<>(returnedEntity, HttpStatus.CREATED);
    }

    protected ResponseEntity<Response> baseUpdate(Request request, Class<Entity> entityClass,
                                                  Class<Response> responseClass) {
        var currentUser = getCurrentUser();
        var updatedEntity = service.update(
                reflectionHelper.convertRequestDtoIntoEntityWhereNestedObjectsWithOnlyValidId(
                        request,
                        entityClass
                ),
                currentUser
        );
        var returnedEntity = modelMapper.map(updatedEntity, responseClass);
        return new ResponseEntity<>(returnedEntity, HttpStatus.OK);
    }

    protected ResponseEntity<Response> baseGetById(UUID id, Class<Response> responseClass) {
        var currentUser = getCurrentUser();
        var foundEntity = service.findByIdAndAccountId(id, currentUser);
        var returnedEntity = modelMapper.map(foundEntity, responseClass);
        return new ResponseEntity<>(returnedEntity, HttpStatus.OK);
    }

    protected ResponseEntity<Page<Response>> baseGetByFilter(Filter filterDto,
                                                             int page,
                                                             int size,
                                                             Class<Entity> entityClass,
                                                             Class<Response> responseClass) {
        var currentUser = getCurrentUser();
        var pageable = createPageable(size, page, filterDto.getSortByFields(), filterDto.getSortDirection());
        var specification =
                specFactory
                        .makeSpecificationFromEntityFilterDto(entityClass, filterDto, currentUser.getAccount().getId());
        var foundEntities = service.findAllByFilter(specification, pageable, currentUser);
        var returnedEntities = mapPage(foundEntities, responseClass, pageable);
        return new ResponseEntity<>(returnedEntities, HttpStatus.OK);
    }

    private User getCurrentUser() {
        var jwtUser = jwtUserBuilder.getJwtUser();
        return userService.findByIdAndAccountId(jwtUser.getId(), jwtUser.getAccountId());
    }
}
