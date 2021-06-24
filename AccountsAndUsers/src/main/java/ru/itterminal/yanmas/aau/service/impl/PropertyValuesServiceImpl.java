package ru.itterminal.yanmas.aau.service.impl;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.yanmas.aau.model.PropertyValues;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.repository.PropertyValuesRepository;
import ru.itterminal.yanmas.aau.service.CrudServiceWithBusinessHandlerImpl;

import java.util.UUID;

@Service
public class PropertyValuesServiceImpl extends CrudServiceWithBusinessHandlerImpl<PropertyValues, PropertyValuesRepository> {
    @Transactional(readOnly = true)
    public Page<PropertyValues> findAllByEntityId(UUID entityId, Pageable pageable, User currentUser) {
        checkAccessBeforeRead(currentUser);
        return repository.findAllByAccountIdAndEntityId
                (currentUser.getAccount().getId(),
                        entityId,
                        pageable
                );
    }
}
