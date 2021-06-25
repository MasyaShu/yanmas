package ru.itterminal.yanmas.aau.repository;

import org.springframework.stereotype.Repository;
import ru.itterminal.yanmas.aau.model.Property;
import ru.itterminal.yanmas.commons.repository.EntityRepositoryWithAccount;

@Repository
public interface PropertyRepository extends EntityRepositoryWithAccount<Property> {
}
