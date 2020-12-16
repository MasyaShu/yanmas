package ru.itterminal.botdesk.files.repository;

import java.util.Optional;
import java.util.UUID;

import org.springframework.stereotype.Repository;

import ru.itterminal.botdesk.commons.repository.EntityRepositoryWithAccount;
import ru.itterminal.botdesk.files.model.File;

@Repository
public interface FileRepository extends EntityRepositoryWithAccount<File> {
    Optional<File> findByAccountIdAndId(UUID accountId, UUID fileId);
}
