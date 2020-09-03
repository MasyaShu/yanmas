package ru.itterminal.botdesk.aau.service.impl;

import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.repository.RoleRepository;
import ru.itterminal.botdesk.aau.service.validator.RoleOperationValidator;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceImpl;

public class RoleServiceImpl extends CrudServiceImpl<Role, RoleOperationValidator, RoleRepository> {
}
