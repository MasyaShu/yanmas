package ru.itterminal.yanmas.aau.service.impl;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.repository.GroupRepository;
import ru.itterminal.yanmas.aau.service.business_handler.impl.CrudServiceWithBusinessHandlerImpl;
import ru.itterminal.yanmas.aau.service.business_handler.impl.GroupBusinessHandlerImpl;
import ru.itterminal.yanmas.aau.service.validator.EntityValidator;

import java.util.List;

@Slf4j
@Service
@Transactional
public class GroupServiceImpl extends CrudServiceWithBusinessHandlerImpl<Group, GroupBusinessHandlerImpl, GroupRepository> {

    public GroupServiceImpl(List<EntityValidator<Group>> validators) {
        this.validators = validators;
    }
}
