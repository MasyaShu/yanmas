package ru.itterminal.yanmas.aau.service.business_handler.group;

import org.springframework.stereotype.Component;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.aau.service.impl.GroupServiceImpl;

@Component
@RequiredArgsConstructor
public class SettingGroupIsInnerFromDataBaseBeforeUpdateGroupBusinessHandler implements EntityBusinessHandler<Group> {

    private final GroupServiceImpl service;

    @Override
    public Group beforeUpdate(Group entity, User currentUser) {
        var groupFromDataBase = service.findByIdAndAccountId(entity.getId(), currentUser);
        entity.setIsInner(groupFromDataBase.getIsInner());
        return entity;
    }
}
