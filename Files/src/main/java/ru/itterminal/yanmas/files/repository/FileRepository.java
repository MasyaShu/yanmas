package ru.itterminal.yanmas.files.repository;

import java.util.Optional;
import java.util.UUID;

import org.springframework.stereotype.Repository;

import ru.itterminal.yanmas.commons.repository.EntityRepositoryWithAccount;
import ru.itterminal.yanmas.files.model.File;

@Repository
public interface FileRepository extends EntityRepositoryWithAccount<File> {
    Optional<File> findByAccountIdAndAuthorIdAndId(UUID accountId, UUID authorId, UUID fileId);
}
