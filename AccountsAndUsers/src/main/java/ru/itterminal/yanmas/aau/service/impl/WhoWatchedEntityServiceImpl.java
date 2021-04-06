package ru.itterminal.yanmas.aau.service.impl;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.service.CrudService.CREATE_FINISH_MESSAGE;
import static ru.itterminal.yanmas.commons.service.CrudService.CREATE_INIT_MESSAGE;
import static ru.itterminal.yanmas.commons.service.CrudService.DELETE_FINISH_MESSAGE;
import static ru.itterminal.yanmas.commons.service.CrudService.DELETE_INIT_MESSAGE;

import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.yanmas.aau.model.WhoWatchedEntity;
import ru.itterminal.yanmas.aau.repository.WhoWatchedEntityRepository;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;

@Slf4j
@Service
@RequiredArgsConstructor
public class WhoWatchedEntityServiceImpl {

    private final WhoWatchedEntityRepository repository;
    private final JwtUserBuilder jwtUserBuilder;

    @Transactional
    public void create(WhoWatchedEntity entity) {
        log.trace(format(CREATE_INIT_MESSAGE, entity.getClass().getSimpleName(), entity.toString()));
        UUID id = UUID.randomUUID();
        entity.setId(id);
        try {
            var createdEntity = repository.save(entity);
            log.trace(format(CREATE_FINISH_MESSAGE, entity.getClass().getSimpleName(), createdEntity.toString()));
        }
        // TODO заменить Exception на исключение "не уникальный индекс"
        catch (Exception exception) {
            log.debug(exception.getMessage());
        }
    }

    @Transactional
    public void delete(WhoWatchedEntity entity) {
        log.trace(format(DELETE_INIT_MESSAGE, entity.toString()));
        try {
            repository.delete(entity);
            log.trace(format(DELETE_FINISH_MESSAGE, entity.toString()));
        }
        catch (Exception exception) {
            log.debug(exception.getMessage());
        }
    }

    public Boolean isWatchedEntity(UUID entityId) {
        var jwtUser = jwtUserBuilder.getJwtUser();
        var accountId = jwtUser.getAccountId();
        var userId = jwtUser.getId();
        return repository.findByAccountIdAndEntityIdAndUserId(accountId, entityId, userId) != null;
    }

    public Map<UUID, Boolean> isWatchedEntities(List<UUID> entitiesId) {
        return null;
    }

    public int getCountUnwatchedEntities(List<UUID> entitiesId) {
        return 0;
    }
}
