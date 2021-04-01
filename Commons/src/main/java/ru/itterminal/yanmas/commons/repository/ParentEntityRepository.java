package ru.itterminal.yanmas.commons.repository;

import java.util.UUID;

import org.springframework.data.repository.NoRepositoryBean;
import org.springframework.data.repository.PagingAndSortingRepository;

import ru.itterminal.yanmas.commons.model.BaseEntity;

@NoRepositoryBean
public interface ParentEntityRepository<T extends BaseEntity> extends PagingAndSortingRepository<T, UUID> {

    T create(T entity);

    T update(T entity);

}
