package ru.itterminal.yanmas.aau.service.impl;

import org.springframework.stereotype.Service;
import ru.itterminal.yanmas.aau.model.PropertyGroup;
import ru.itterminal.yanmas.aau.repository.PropertyGroupRepository;
import ru.itterminal.yanmas.aau.service.CrudServiceWithBusinessHandlerImpl;

@Service
public class PropertyGroupServiceImpl extends CrudServiceWithBusinessHandlerImpl<PropertyGroup, PropertyGroupRepository> {
}
