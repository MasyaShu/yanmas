package ru.itterminal.yanmas.aau.repository;

import java.util.List;
import java.util.UUID;

import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

import ru.itterminal.yanmas.aau.model.WhoWatchedEntity;

@Repository
public interface WhoWatchedEntityRepository extends CrudRepository<WhoWatchedEntity, UUID> {

    WhoWatchedEntity findByAccountIdAndEntityIdAndUserId(UUID accountId, UUID entityId, UUID userId);
    List<WhoWatchedEntity> findAllByAccountIdAndEntityIdInAndUserId(UUID accountId, List<UUID> entityId, UUID userId);
    void deleteByAccountIdAndEntityIdAndUserId(UUID accountId, UUID entityId, UUID userId);
}
