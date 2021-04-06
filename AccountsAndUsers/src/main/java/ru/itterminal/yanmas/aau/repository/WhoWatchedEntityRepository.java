package ru.itterminal.yanmas.aau.repository;

import java.util.UUID;

import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

import ru.itterminal.yanmas.aau.model.WhoWatchedEntity;

@Repository
public interface WhoWatchedEntityRepository extends CrudRepository<WhoWatchedEntity, UUID> {

    WhoWatchedEntity findByAccountIdAndEntityIdAndUserId(UUID accountId, UUID entityId, UUID userId);
}
