package ru.itterminal.yanmas.aau.service.impl;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import ru.itterminal.yanmas.aau.model.Role;
import ru.itterminal.yanmas.aau.model.Roles;
import ru.itterminal.yanmas.aau.repository.RoleRepository;
import ru.itterminal.yanmas.aau.service.validator.account.RoleOperationValidator;
import ru.itterminal.yanmas.commons.exception.EntityNotExistException;
import ru.itterminal.yanmas.commons.service.crud.impl.CrudServiceImpl;

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

    /**
     * @deprecated - Role is a immutable entity
     */
    @Override
    @Deprecated(since = "the beginning of time")
    public Role create(Role entity) {
        throw new UnsupportedOperationException(METHOD_NOT_SUPPORTED);
    }

    /**
     * @deprecated - Role is a immutable entity
     */
    @Override
    @Deprecated(since = "the beginning of time")
    public Role update(Role entity) {
        throw new UnsupportedOperationException(METHOD_NOT_SUPPORTED);
    }
}
