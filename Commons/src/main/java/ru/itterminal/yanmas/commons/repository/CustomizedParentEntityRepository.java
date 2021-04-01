package ru.itterminal.yanmas.commons.repository;

import java.util.UUID;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.NoRepositoryBean;
import org.springframework.data.repository.query.Param;

import ru.itterminal.yanmas.commons.model.BaseEntity;

@SuppressWarnings("JpaQlInspection")
@NoRepositoryBean
public interface CustomizedParentEntityRepository<T extends BaseEntity> extends ParentEntityRepository<T>,
    JpaSpecificationExecutor<T> {

    @Query("select t from #{#entityName} t where t.deleted = :deleted")
    Page<T> findMarked(Pageable pageable, @Param("deleted") boolean deleted);

    @Modifying
    @Query("update #{#entityName} set deleted = :deleted, version = (:version + 1) " +
        "where id = :id and version = :version")
    int logicalDelete(@Param("id") UUID id, @Param("deleted") boolean deleted, @Param("version") int version);

}
