package ru.itterminal.yanmas.aau.util;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import javax.validation.ConstraintViolationException;

import org.modelmapper.ModelMapper;
import org.springframework.context.ApplicationContext;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.model.Account;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.CrudServiceWithAccount;
import ru.itterminal.yanmas.aau.service.business_handler.CrudServiceWithBusinessHandler;
import ru.itterminal.yanmas.commons.exception.EntityNotExistException;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.commons.service.crud.CrudService;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;

@SuppressWarnings("rawtypes")
@Component
@RequiredArgsConstructor
public class ReflectionHelper {

    public static final String SERVICE_IMPL = "ServiceImpl";
    private final ModelMapper modelMapper;
    private final JwtUserBuilder jwtUserBuilder;
    private final ApplicationContext appContext;

    public static final String ACCOUNT = "account";

    @SuppressWarnings("unchecked")
    public BaseEntity convertRequestDtoIntoEntityWhereNestedObjectsWithOnlyValidId
            (Object request, Class<? extends BaseEntity> entityClazz) {
        try {
            var requestClazz = request.getClass();
            var returnedEntity = modelMapper.map(request, entityClazz);
            var fieldsReturnedEntity = returnedEntity.getClass().getDeclaredFields();
            var accountId = jwtUserBuilder.getJwtUser().getAccountId();
            for (Field fieldEntity : fieldsReturnedEntity) {
                if (fieldEntity.getName().equals(ACCOUNT)) {
                    fieldEntity.setAccessible(true); //NOSONAR
                    fieldEntity.set(returnedEntity, Account.builder().id(accountId).build()); //NOSONAR
                }
                if (fieldEntity.getType().isAssignableFrom(List.class)) {
                    var accordingFieldOfRequest = requestClazz.getDeclaredField(fieldEntity.getName());
                    accordingFieldOfRequest.setAccessible(true); //NOSONAR
                    var listFromRequest = (List<UUID>) accordingFieldOfRequest.get(request);
                    if (listFromRequest != null && !listFromRequest.isEmpty()) {
                        var typeEntityFromList = (Class<? extends BaseEntity>)
                                ((ParameterizedType) fieldEntity.getGenericType()).getActualTypeArguments()[0];
                        var listOfEntities = new ArrayList<>();
                        for (UUID uuid : listFromRequest) {
                            var newEntity = typeEntityFromList.getDeclaredConstructor().newInstance();
                            newEntity.setId(uuid);
                            listOfEntities.add(newEntity);
                        }
                        fieldEntity.setAccessible(true); //NOSONAR
                        fieldEntity.set(returnedEntity, listOfEntities); //NOSONAR
                    } else {
                        fieldEntity.setAccessible(true); //NOSONAR
                        fieldEntity.set(returnedEntity, null); //NOSONAR
                    }
                }
            }
            return returnedEntity;
        }
        catch (Exception e) {
            throw new RuntimeException(e.getCause()); //NOSONAR
        }
    }

    public void settingNestedObjectsIntoEntity(Object entity, User currentUser) {
        try {
            var fieldsOfEntity = entity.getClass().getDeclaredFields();
            for (Field fieldEntity : fieldsOfEntity) {
                fieldEntity.setAccessible(true);
                if (fieldEntity.get(entity) != null) {
                    if (fieldEntity.getType().isAssignableFrom(List.class)) {
                        setNestedListOfEntity(entity, currentUser, fieldEntity);
                    } else if (fieldEntity.getType().getGenericSuperclass().equals(BaseEntity.class)) {
                        setNestedEntity(entity, currentUser, fieldEntity);
                    }
                }
            }
        }
        catch (AccessDeniedException | EntityNotExistException | ConstraintViolationException e) {
            throw e;
        }
        catch (Throwable e) {
            throw new RuntimeException(e.getCause()); //NOSONAR
        }
    }

    void setNestedListOfEntity(Object entity, User currentUser, Field fieldEntity) throws Throwable {
        var typeObjectFromList =
                ((ParameterizedType) fieldEntity.getGenericType()).getActualTypeArguments()[0];
        if (((Class) typeObjectFromList).getGenericSuperclass().equals(BaseEntity.class)) {
            var typeOfObjectFromList = ((Class<?>) typeObjectFromList).getSimpleName();
            var nameOfEntityService =
                    typeOfObjectFromList.substring(0, 1).toLowerCase() + typeOfObjectFromList.substring(1)
                            + SERVICE_IMPL;
            @SuppressWarnings("unchecked")
            var listOfEntities = (List<BaseEntity>) fieldEntity.get(entity);
            var listOfEntitiesFromDatabase = new ArrayList<>();
            for (BaseEntity entityFromList : listOfEntities) {
                try {
                    var service = (CrudServiceWithBusinessHandler) appContext.getBean(nameOfEntityService);
                    var entityFromDatabase = service.findByIdAndAccountId(
                            entityFromList.getId(),
                            currentUser
                    );
                    listOfEntitiesFromDatabase.add(entityFromDatabase);
                }
                //  TODO delete
                catch (ClassCastException e) {
                    var service = (CrudServiceWithAccount) appContext.getBean(nameOfEntityService);
                    var entityFromDatabase = service.findByIdAndAccountId(
                            entityFromList.getId(),
                            currentUser.getAccount().getId()
                    );
                    listOfEntitiesFromDatabase.add(entityFromDatabase);

                }
            }
            fieldEntity.set(entity, listOfEntitiesFromDatabase);
        }
    }

    void setNestedEntity(Object entity, User currentUser, Field fieldEntity) throws Throwable {
        var nestedEntity = (BaseEntity) fieldEntity.get(entity);
        var typeOfNestedEntity = fieldEntity.getName();
        var nameOfEntityService = typeOfNestedEntity.substring(0, 1).toLowerCase() + typeOfNestedEntity.substring(1)
                + SERVICE_IMPL;
        try {
            var service = (CrudServiceWithBusinessHandler) appContext.getBean(nameOfEntityService);
            var entityFromDatabase = service.findByIdAndAccountId(
                    nestedEntity.getId(),
                    currentUser
            );
            fieldEntity.set(entity, entityFromDatabase);
        }
        catch (ClassCastException e) {
            // TODO delete
            System.out.println(e.getMessage());
        }
        try { //NOSONAR
            // TODO delete
            var service = (CrudServiceWithAccount) appContext.getBean(nameOfEntityService);
            var entityFromDatabase = service.findByIdAndAccountId(
                    nestedEntity.getId(),
                    currentUser.getAccount().getId()
            );
            fieldEntity.set(entity, entityFromDatabase);
        }
        catch (ClassCastException e) {
            var serviceRole = (CrudService) appContext.getBean(nameOfEntityService);
            var entityFromDatabase = serviceRole.findById(nestedEntity.getId());
            fieldEntity.set(entity, entityFromDatabase);
        }
    }
}
