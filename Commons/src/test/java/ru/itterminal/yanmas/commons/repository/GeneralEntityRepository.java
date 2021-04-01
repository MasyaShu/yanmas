package ru.itterminal.yanmas.commons.repository;


import org.springframework.stereotype.Repository;

import ru.itterminal.yanmas.commons.model.GeneralEntity;

/**
 * Test repository that include all general methods
 * Use only for junit general entity repository tests.
 */
@Repository
public interface GeneralEntityRepository extends CustomizedParentEntityRepository<GeneralEntity> {
}
