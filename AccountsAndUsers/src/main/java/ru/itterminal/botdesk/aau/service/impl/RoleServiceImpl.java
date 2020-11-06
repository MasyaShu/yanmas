package ru.itterminal.botdesk.aau.service.impl;

import org.springframework.stereotype.Service;

import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.repository.RoleRepository;
import ru.itterminal.botdesk.aau.service.validator.RoleOperationValidator;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceImpl;

@Service
public class RoleServiceImpl extends CrudServiceImpl<Role, RoleOperationValidator, RoleRepository> {

    private Role accountOwnerRole;

    public Role getAccountOwnerRole() {
        if (accountOwnerRole == null) {
            accountOwnerRole = repository.getByName(Roles.ACCOUNT_OWNER.toString()).get();
        }
        return accountOwnerRole;
    }
}
