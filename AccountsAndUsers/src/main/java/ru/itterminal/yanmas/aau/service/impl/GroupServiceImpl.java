package ru.itterminal.yanmas.aau.service.impl;

import org.springframework.stereotype.Service;

import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.repository.GroupRepository;
import ru.itterminal.yanmas.aau.service.CrudServiceWithBusinessHandlerImpl;

@Service
public class GroupServiceImpl extends CrudServiceWithBusinessHandlerImpl<Group, GroupRepository> {
}
