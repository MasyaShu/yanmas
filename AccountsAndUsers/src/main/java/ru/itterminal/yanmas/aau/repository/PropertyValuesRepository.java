package ru.itterminal.yanmas.aau.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import ru.itterminal.yanmas.aau.model.PropertyValues;
import ru.itterminal.yanmas.commons.repository.EntityRepositoryWithAccount;

import java.util.UUID;

@Repository
public interface PropertyValuesRepository extends EntityRepositoryWithAccount<PropertyValues> {
    @Query(value = "SELECT properties.*,\n" +
            "       values.value AS values\n" +
            "FROM properties\n" +
            "LEFT JOIN\n" +
            "    (SELECT property_values.value,\n" +
            "            property_values.property_id\n" +
            "           FROM property_values\n" +
            "           WHERE property_values.entity_id = ?) AS values\n" +
            "        ON properties.id = values.property_id\n" +
            "WHERE entity_name = ? AND account_id = ?",
            nativeQuery = true)
    Page<PropertyValues> findAllPropertiesWithValues(UUID entityId, String nameEntity, UUID accountId, Pageable pageable);
}
