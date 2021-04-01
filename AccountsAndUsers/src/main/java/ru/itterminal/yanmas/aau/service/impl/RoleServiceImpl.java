package ru.itterminal.yanmas.aau.service.impl;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import ru.itterminal.yanmas.aau.model.Role;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.repository.RoleRepository;
import ru.itterminal.yanmas.aau.service.validator.RoleOperationValidator;
import ru.itterminal.yanmas.commons.exception.EntityNotExistException;
import ru.itterminal.yanmas.commons.service.impl.CrudServiceImpl;

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
