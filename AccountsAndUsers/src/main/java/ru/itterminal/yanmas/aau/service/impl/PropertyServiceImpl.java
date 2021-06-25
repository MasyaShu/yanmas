package ru.itterminal.yanmas.aau.service.impl;

import org.springframework.stereotype.Service;
import ru.itterminal.yanmas.aau.model.Property;
import ru.itterminal.yanmas.aau.repository.PropertyRepository;
import ru.itterminal.yanmas.aau.service.CrudServiceWithBusinessHandlerImpl;

@Service
public class PropertyServiceImpl extends CrudServiceWithBusinessHandlerImpl<Property, PropertyRepository> {
}
