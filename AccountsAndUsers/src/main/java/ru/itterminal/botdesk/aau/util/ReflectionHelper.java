package ru.itterminal.botdesk.aau.util;

import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.modelmapper.ModelMapper;
import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.commons.model.BaseEntity;
import ru.itterminal.botdesk.security.jwt.JwtUserBuilder;

@Component
@RequiredArgsConstructor
public class ReflectionHelper {

    private final ModelMapper modelMapper;
    private final JwtUserBuilder jwtUserBuilder;

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
                    fieldEntity.setAccessible(true);
                    fieldEntity.set(returnedEntity, Account.builder().id(accountId).build());
                }
                if (fieldEntity.getType().isAssignableFrom(List.class)) {
                    var accordingFieldOfRequest = requestClazz.getDeclaredField(fieldEntity.getName());
                    accordingFieldOfRequest.setAccessible(true);
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
                        fieldEntity.setAccessible(true);
                        fieldEntity.set(returnedEntity, listOfEntities);
                    } else {
                        fieldEntity.setAccessible(true);
                        fieldEntity.set(returnedEntity, null);
                    }
                }
            }
            return returnedEntity;
        }
        catch (Exception e) {
            throw new RuntimeException(e.getCause());
        }
    }
}
