package ru.itterminal.botdesk.aau.model.test;

import java.util.List;
import java.util.UUID;

import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.dto.GroupDto;
import ru.itterminal.botdesk.aau.model.dto.GroupFilterDto;
import ru.itterminal.botdesk.commons.model.BaseTestEntityHelperImpl;

public class GroupTestHelper extends BaseTestEntityHelperImpl<Group, GroupDto, GroupFilterDto> {

    private static final String INVALID_GROUP_NAME_REGEX = "[A-Za-z0-9]{129}";
    private final AccountTestHelper accountHelper = new AccountTestHelper();

    @Override
    public Group getRandomValidEntity() {
        Group group = Group.builder()
                .name(fakerRU.hipster().word())
                .comment(fakerRU.lorem().paragraph())
                .isInner(fakerRU.bool().bool())
                .isDeprecated(fakerRU.bool().bool())
                .account(accountHelper.getRandomValidEntity())
                .build();
        setRandomValidPropertiesOfBaseEntity(group);
        return group;
    }

    @Override
    public Group getRandomInvalidEntity() {
        Group group = Group.builder()
                .name(fakerRU.regexify(INVALID_GROUP_NAME_REGEX))
                .comment(fakerRU.lorem().paragraph())
                .isInner(fakerRU.bool().bool())
                .isDeprecated(fakerRU.bool().bool())
                .account(accountHelper.getRandomValidEntity())
                .build();
        setRandomValidPropertiesOfBaseEntity(group);
        return group;
    }

    @Override
    public List<Group> setPredefinedValidEntityList() {
        Group group1 = Group.builder()
                .name("groupName1")
                .comment("comment for group1 of users")
                .account(accountHelper.getPredefinedValidEntityList().get(0))
                .isDeprecated(false)
                .isInner(true)
                .build();
        setPropertiesOfBaseEntity(
                group1,
                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                0,
                false,
                null
        );
        Group group2 = Group.builder()
                .name("groupName3")
                .comment("")
                .account(accountHelper.getPredefinedValidEntityList().get(0))
                .isDeprecated(true)
                .isInner(false)
                .build();
        setPropertiesOfBaseEntity(
                group2,
                UUID.fromString("63c1940d-e323-47af-8265-dbf8089727de"),
                0,
                false,
                ""
        );
        Group group3 = Group.builder()
                .name("groupName2")
                .comment(null)
                .account(accountHelper.getPredefinedValidEntityList().get(1))
                .isDeprecated(false)
                .isInner(false)
                .build();
        setPropertiesOfBaseEntity(
                group3,
                UUID.fromString("99f6b488-7687-4451-b8a1-9fbeb2a3efec"),
                0,
                false,
                null
        );
        return List.of(group1, group2, group3);
    }

    @Override
    public GroupDto getRandomValidEntityDto() {
        GroupDto groupDto = GroupDto.builder()
                .name(fakerRU.hipster().word())
                .comment(fakerRU.lorem().paragraph())
                .isInner(fakerRU.bool().bool())
                .isDeprecated(fakerRU.bool().bool())
                .build();
        setRandomValidPropertiesOfBaseEntityDto(groupDto);
        return groupDto;
    }

    @Override
    public GroupDto getRandomInvalidEntityDto() {
        GroupDto groupDto = GroupDto.builder()
                .name(fakerRU.regexify(INVALID_GROUP_NAME_REGEX))
                .comment(fakerRU.lorem().paragraph())
                .isInner(fakerRU.bool().bool())
                .isDeprecated(fakerRU.bool().bool())
                .build();
        setRandomValidPropertiesOfBaseEntityDto(groupDto);
        return groupDto;    }
}