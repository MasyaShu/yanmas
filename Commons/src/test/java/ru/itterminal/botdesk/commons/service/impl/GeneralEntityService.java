package ru.itterminal.botdesk.commons.service.impl;

import java.util.UUID;

import ru.itterminal.botdesk.commons.model.GeneralEntity;
import ru.itterminal.botdesk.commons.repository.GeneralEntityRepository;
import ru.itterminal.botdesk.commons.service.validator.impl.BasicOperationValidatorImpl;

class GeneralEntityService extends CrudServiceImpl<GeneralEntity, BasicOperationValidatorImpl<GeneralEntity>,
        GeneralEntityRepository> {

}
