package ru.itterminal.yanmas.aau.service.impl;

import static java.lang.String.format;
import static ru.itterminal.yanmas.commons.service.CrudService.CREATE_FINISH_MESSAGE;
import static ru.itterminal.yanmas.commons.service.CrudService.CREATE_INIT_MESSAGE;
import static ru.itterminal.yanmas.commons.service.CrudService.DELETE_FINISH_MESSAGE;
import static ru.itterminal.yanmas.commons.service.CrudService.DELETE_INIT_MESSAGE;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import ru.itterminal.yanmas.aau.model.WhoWatchedEntity;
import ru.itterminal.yanmas.aau.repository.WhoWatchedEntityRepository;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;

@SuppressWarnings("unused")
@Slf4j
@Service
@RequiredArgsConstructor
public class WhoWatchedEntityServiceImpl {

    private final WhoWatchedEntityRepository repository;
    private final JwtUserBuilder jwtUserBuilder;
    private final String entityName = WhoWatchedEntity.class.getSimpleName();

    @Transactional
    public void watched(List<UUID> entitiesId) {
        var jwtUser = jwtUserBuilder.getJwtUser();
        var accountId = jwtUser.getAccountId();
        var userId = jwtUser.getId();
        var entitiesIdWithoutDuplicates = new ArrayList<>(new HashSet<>(entitiesId));
        for (UUID entityId : entitiesIdWithoutDuplicates) {
            var entity = WhoWatchedEntity.builder()
                    .id(UUID.randomUUID())
                    .accountId(accountId)
                    .entityId(entityId)
                    .userId(userId)
                    .build();
            var foundEntity = repository.findByAccountIdAndEntityIdAndUserId(accountId, entityId, userId);
            if (foundEntity != null) {
                continue;
            }
            log.trace(format(CREATE_INIT_MESSAGE, entityName, entity.toString()));
            var createdEntity = repository.save(entity);
            log.trace(format(CREATE_FINISH_MESSAGE, entity.getClass().getSimpleName(), createdEntity.toString()));
        }
    }

    @Transactional
    public void unwatched(List<UUID> entitiesId) {
        var jwtUser = jwtUserBuilder.getJwtUser();
        var accountId = jwtUser.getAccountId();
        var userId = jwtUser.getId();
        var entitiesIdWithoutDuplicates = new ArrayList<>(new HashSet<>(entitiesId));
        for (UUID entityId : entitiesIdWithoutDuplicates) {
            log.trace(format(DELETE_INIT_MESSAGE, entityId.toString()));
            try {
                repository.deleteByAccountIdAndEntityIdAndUserId(accountId, entityId, userId);
                log.trace(format(DELETE_FINISH_MESSAGE, entityId.toString()));
            }
            catch (Exception exception) {
                log.debug(exception.getMessage());
            }
        }
    }

    @Transactional(readOnly = true)
    public Boolean isWatchedEntity(UUID entityId) {
        var jwtUser = jwtUserBuilder.getJwtUser();
        var accountId = jwtUser.getAccountId();
        var userId = jwtUser.getId();
        return repository.findByAccountIdAndEntityIdAndUserId(accountId, entityId, userId) != null;
    }

    @Transactional(readOnly = true)
    public Map<UUID, Boolean> isWatchedEntities(List<UUID> entitiesId) {
        var jwtUser = jwtUserBuilder.getJwtUser();
        var accountId = jwtUser.getAccountId();
        var userId = jwtUser.getId();
        var watchedEntityList = repository.findAllByAccountIdAndEntityIdInAndUserId(accountId, entitiesId, userId);
        var watchedEntityIdList = watchedEntityList.stream()
                .map(WhoWatchedEntity::getEntityId)
                .collect(Collectors.toList());
        return entitiesId.stream()
                .collect(Collectors.toMap(
                        entityId -> entityId,
                        watchedEntityIdList::contains,
                        (oldValue, newValue) -> oldValue
                ));
    }

    @Transactional(readOnly = true)
    public int getCountUnwatchedEntities(List<UUID> entitiesId) {
        var jwtUser = jwtUserBuilder.getJwtUser();
        var accountId = jwtUser.getAccountId();
        var userId = jwtUser.getId();
        var entitiesIdWithoutDuplicates = new ArrayList<>(new HashSet<>(entitiesId));
        var countTotalEntities = entitiesIdWithoutDuplicates.size();
        var watchedEntityList =
                repository.findAllByAccountIdAndEntityIdInAndUserId(accountId, entitiesIdWithoutDuplicates, userId);
        var countWatchedEntities = watchedEntityList.size();
        return countTotalEntities - countWatchedEntities;
    }
}
