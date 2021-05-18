package ru.itterminal.yanmas.aau.service.business_handler.impl;

import lombok.RequiredArgsConstructor;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.aau.service.impl.GroupServiceImpl;
import ru.itterminal.yanmas.commons.model.filter.StringFilter;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;

import static ru.itterminal.yanmas.commons.model.filter.StringFilter.TypeComparisonForStringFilter.TEXT_EQUALS;

@Component
@RequiredArgsConstructor
public class GroupBusinessHandlerImpl implements EntityBusinessHandler<Group> {

    private final GroupServiceImpl service;
    private final SpecificationsFactory specFactory;

    @Override
    public void beforeUpdate(Group entity, User currentUser) {
        var groupFromDataBase = service.findByIdAndAccountId(entity.getId(), currentUser);
        entity.setIsInner(groupFromDataBase.getIsInner());
    }
}
