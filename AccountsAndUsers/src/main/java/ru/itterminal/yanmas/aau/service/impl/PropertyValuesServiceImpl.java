package ru.itterminal.yanmas.aau.service.impl;

import org.springframework.stereotype.Service;
import ru.itterminal.yanmas.aau.model.PropertyValues;
import ru.itterminal.yanmas.aau.repository.PropertyValuesRepository;
import ru.itterminal.yanmas.aau.service.CrudServiceWithBusinessHandlerImpl;

@Service
public class PropertyValuesServiceImpl extends CrudServiceWithBusinessHandlerImpl<PropertyValues, PropertyValuesRepository> {
}
