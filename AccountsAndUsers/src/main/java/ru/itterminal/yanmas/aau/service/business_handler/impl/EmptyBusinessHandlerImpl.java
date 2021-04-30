package ru.itterminal.yanmas.aau.service.business_handler.impl;

import org.springframework.stereotype.Component;

import ru.itterminal.yanmas.aau.service.business_handler.EntityBusinessHandler;
import ru.itterminal.yanmas.commons.model.BaseEntity;

@Component
public class EmptyBusinessHandlerImpl<E extends BaseEntity> implements EntityBusinessHandler<E> {
}
