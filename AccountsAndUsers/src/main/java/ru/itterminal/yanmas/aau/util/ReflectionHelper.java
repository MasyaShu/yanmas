package ru.itterminal.yanmas.aau.util;

import static java.lang.String.format;
import static ru.itterminal.yanmas.aau.service.business_handler.CrudServiceWithBusinessHandler.FIND_INVALID_MESSAGE;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.modelmapper.ModelMapper;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.model.Account;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.commons.exception.EntityNotExistException;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.commons.repository.CustomizedParentEntityRepository;
import ru.itterminal.yanmas.commons.repository.EntityRepositoryWithAccount;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;

@SuppressWarnings({"rawtypes", "unchecked"})
@Component
@RequiredArgsConstructor
public class ReflectionHelper {

    public static final String REPOSITORY = "Repository";
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
        catch (RuntimeException e) {
            throw e;
        }
        catch (Throwable e) {
            throw new RuntimeException(e.getCause());
        }
    }

    void setNestedListOfEntity(Object entity, User currentUser, Field fieldEntity) throws Throwable {
        var typeObjectFromList =
                ((ParameterizedType) fieldEntity.getGenericType()).getActualTypeArguments()[0];
        if (((Class) typeObjectFromList).getGenericSuperclass().equals(BaseEntity.class)) {
            var typeOfObjectFromList = ((Class<?>) typeObjectFromList).getSimpleName();
            var nameOfEntityRepository =
                    typeOfObjectFromList.substring(0, 1).toLowerCase() + typeOfObjectFromList.substring(1)
                            + REPOSITORY;
            var listOfEntities = (List<BaseEntity>) fieldEntity.get(entity);
            var listOfEntitiesFromDatabase = new ArrayList<>();
            for (BaseEntity entityFromList : listOfEntities) {
                try {
                    var repository = (EntityRepositoryWithAccount) appContext.getBean(nameOfEntityRepository);
                    var entityFromDatabase = repository.findByIdAndAccountId(
                            entityFromList.getId(),
                            currentUser.getAccount().getId()
                    ).orElseThrow(
                            () -> new EntityNotExistException(
                                    format(FIND_INVALID_MESSAGE, entityFromList.getId(), typeOfObjectFromList)
                            )
                    );
                    listOfEntitiesFromDatabase.add(entityFromDatabase);
                }
                catch (ClassCastException e) {
                    var repository = (CustomizedParentEntityRepository) appContext.getBean(nameOfEntityRepository);
                    var entityFromDatabase = repository.findById(entityFromList.getId()
                    ).orElseThrow(
                            () -> new EntityNotExistException(
                                    format(FIND_INVALID_MESSAGE, entityFromList.getId(), typeOfObjectFromList)
                            )
                    );
                    listOfEntitiesFromDatabase.add(entityFromDatabase);
                }
            }
            fieldEntity.set(entity, listOfEntitiesFromDatabase);
        }
    }

    void setNestedEntity(Object entity, User currentUser, Field fieldEntity) throws Throwable {
        var nestedEntity = (BaseEntity) fieldEntity.get(entity);
        var typeOfNestedEntity = fieldEntity.getType().getSimpleName();
        var nameOfEntityRepository = typeOfNestedEntity.substring(0, 1).toLowerCase() + typeOfNestedEntity.substring(1)
                + REPOSITORY;
        try {
            var repository = (EntityRepositoryWithAccount) appContext.getBean(nameOfEntityRepository);
            var entityFromDatabase = repository.findByIdAndAccountId(
                    nestedEntity.getId(),
                    currentUser.getAccount().getId()
            ).orElseThrow(
                    () -> new EntityNotExistException(
                            format(FIND_INVALID_MESSAGE, nestedEntity.getId(), typeOfNestedEntity)
                    )
            );
            fieldEntity.set(entity, entityFromDatabase);
        }
        catch (ClassCastException e) {
            var repository = (CustomizedParentEntityRepository) appContext.getBean(nameOfEntityRepository);
            var entityFromDatabase = repository.findById(nestedEntity.getId()).orElseThrow(
                    () -> new EntityNotExistException(
                            format(FIND_INVALID_MESSAGE, nestedEntity.getId(), typeOfNestedEntity)
                    )
            );
            fieldEntity.set(entity, entityFromDatabase);

        }
    }
}
