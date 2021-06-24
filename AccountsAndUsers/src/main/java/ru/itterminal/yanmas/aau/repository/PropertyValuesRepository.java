package ru.itterminal.yanmas.aau.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;
import ru.itterminal.yanmas.aau.model.PropertyValues;
import ru.itterminal.yanmas.commons.repository.EntityRepositoryWithAccount;

import java.util.UUID;

@Repository
public interface PropertyValuesRepository extends EntityRepositoryWithAccount<PropertyValues> {
Page<PropertyValues> findAllByAccountIdAndEntityId(UUID accountId, UUID entityId, Pageable pageable);
}
