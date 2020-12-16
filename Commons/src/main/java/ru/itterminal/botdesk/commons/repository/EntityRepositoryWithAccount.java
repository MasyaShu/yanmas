package ru.itterminal.botdesk.commons.repository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.NoRepositoryBean;
import org.springframework.data.repository.query.Param;

import ru.itterminal.botdesk.commons.model.BaseEntity;

@SuppressWarnings("JpaQlInspection")
@NoRepositoryBean
public interface EntityRepositoryWithAccount<T extends BaseEntity> extends CustomizedParentEntityRepository<T> {

    @Query("select t from #{#entityName} t where t.account.id = :accountId")
    List<T> findAllByAccountId(@Param("accountId") UUID accountId);

    @Query("select t from #{#entityName} t where t.id = :id and t.account.id = :accountId")
    Optional<T> findByIdAndAccountId(@Param("id") UUID id, @Param("accountId") UUID accountId);

    @Query("select t from #{#entityName} t where t.account.id = :accountId and t.id in :listId")
    List<T> findAllByAccountIdAndListId(@Param("accountId") UUID accountId, @Param("listId") List<UUID> listId);
}
