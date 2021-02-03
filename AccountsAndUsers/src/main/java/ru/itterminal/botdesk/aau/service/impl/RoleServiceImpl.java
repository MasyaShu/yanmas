package ru.itterminal.botdesk.aau.service.impl;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.repository.RoleRepository;
import ru.itterminal.botdesk.aau.service.validator.RoleOperationValidator;
import ru.itterminal.botdesk.commons.exception.EntityNotExistException;
import ru.itterminal.botdesk.commons.service.impl.CrudServiceImpl;

import static java.lang.String.format;

@Slf4j
@Service
public class RoleServiceImpl extends CrudServiceImpl<Role, RoleOperationValidator, RoleRepository> {

    public static final String METHOD_NOT_SUPPORTED = "Method not supported";
    private Role accountOwnerRole;

    public Role getAccountOwnerRole() {
        if (accountOwnerRole == null) {
            accountOwnerRole = repository.getByName(Roles.ACCOUNT_OWNER.toString()).orElseThrow(() -> {
                String message = format(FIND_INVALID_MESSAGE, "name", Roles.ACCOUNT_OWNER.toString());
                log.error(message);
                return new EntityNotExistException(message);
            });
        }
        return accountOwnerRole;
    }

    @Override
    @Deprecated
    public Role create(Role entity) {
        throw new UnsupportedOperationException(METHOD_NOT_SUPPORTED);
    }

    @Override
    @Deprecated
    public Role update(Role entity) {
        throw new UnsupportedOperationException(METHOD_NOT_SUPPORTED);
    }
}
