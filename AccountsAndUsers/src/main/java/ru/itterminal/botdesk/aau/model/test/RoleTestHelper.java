package ru.itterminal.botdesk.aau.model.test;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import ru.itterminal.botdesk.aau.model.Role;
import ru.itterminal.botdesk.aau.model.Roles;
import ru.itterminal.botdesk.aau.model.dto.RoleDto;
import ru.itterminal.botdesk.commons.model.EntityTestHelperImpl;

public class RoleTestHelper extends EntityTestHelperImpl<Role, RoleDto,RoleDto> {

    private final String[] rolesName = new String[] {Roles.ACCOUNT_OWNER.toString(),
            Roles.ADMIN.toString(), Roles.EXECUTOR.toString(), Roles.AUTHOR.toString(),
            Roles.OBSERVER.toString()};

    private final UUID[] rolesId = new UUID[] {UUID.fromString("ba99ce38-1611-4a81-adc9-3a779d58bbfe"),
            UUID.fromString("607f04b1-f5f9-4f20-9c6f-501c32d773c0"),
            UUID.fromString("f7e579e6-0609-467a-91ff-454f42da3d58"),
            UUID.fromString("933f20bf-9262-47bb-83d2-0ca55bbbd3fd"),
            UUID.fromString("586e087f-f5a0-4db8-af57-edead19db706")};

    private final int[] rolesWeight = new int[] {50, 40, 30, 20, 10};

    @Override
    public Role getRandomValidEntity() {
        int index = fakerEN.number().numberBetween(0, 5);
        Role role = Role.builder()
                .weight(rolesWeight[index])
                .name(rolesName[index])
                .build();
        setPropertiesOfBaseEntity(role, rolesId[index], 0, false, null);
        return role;
    }

    @Override
    public List<Role> setPredefinedValidEntityList() {
        List<Role> roleList = new ArrayList<>();
        for (int i = 0; i < 5; i++) {
            Role role = Role.builder()
                    .weight(rolesWeight[i])
                    .name(rolesName[i])
                    .build();
            setPropertiesOfBaseEntity(role, rolesId[i], 0, false, null);
            roleList.add(role);
        }
        return roleList;
    }

}
