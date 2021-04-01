package ru.itterminal.yanmas.aau.model.test;

import java.util.List;
import java.util.UUID;

import org.modelmapper.ModelMapper;

import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.dto.GroupDto;
import ru.itterminal.yanmas.aau.model.dto.GroupFilterDto;
import ru.itterminal.yanmas.commons.model.EntityTestHelperImpl;
import ru.itterminal.yanmas.commons.model.filter.BooleanFilter;
import ru.itterminal.yanmas.commons.model.filter.StringFilter;

import static ru.itterminal.yanmas.commons.model.filter.StringFilter.TypeComparisonForStringFilter.TEXT_EQUALS;

public class GroupTestHelper extends EntityTestHelperImpl<Group, GroupDto, GroupDto> {

    private final AccountTestHelper accountHelper = new AccountTestHelper();
    private final ModelMapper modelMapper = new ModelMapper();

    @Override
    public GroupDto convertEntityToDtoRequest(Group group, boolean isDtoForCreate) {
        var groupDto = modelMapper.map(group, GroupDto.class);
        if (isDtoForCreate) {
            groupDto.setId(null);
            groupDto.setDeleted(null);
            groupDto.setVersion(null);
            groupDto.setIsDeprecated(null);
        } else {
            groupDto.setIsInner(null);
        }
        groupDto.setDisplayName(null);
        return groupDto;
    }

    @Override
    public Group getRandomValidEntity() {
        Group group = Group.builder()
                .name(fakerRU.name().fullName())
                .comment(fakerRU.lorem().paragraph())
                .isInner(fakerRU.bool().bool())
                .isDeprecated(fakerRU.bool().bool())
                .account(accountHelper.getRandomValidEntity())
                .build();
        setRandomValidPropertiesOfBaseEntity(group);
        group.generateDisplayName();
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
        group1.generateDisplayName();
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
        group2.generateDisplayName();
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
        group3.generateDisplayName();
        return List.of(group1, group2, group3);
    }

    public GroupFilterDto convertEntityToFilterDto(Group group) {
        var filterDto = GroupFilterDto.builder()
                .name(StringFilter.builder()
                        .typeComparison(TEXT_EQUALS.toString())
                        .value(group.getName())
                        .build())
                .isDeprecated(BooleanFilter.builder()
                        .value(group.getIsDeprecated())
                        .build())
                .isInner(BooleanFilter.builder()
                        .value(group.getIsInner())
                        .build())
                .deleted(BooleanFilter.builder()
                        .value(group.getDeleted())
                        .build())
                .build();

        if (group.getOutId() != null && !group.getOutId().isEmpty()) {
            filterDto.setOutId(StringFilter.builder()
                    .value(group.getOutId())
                    .typeComparison(TEXT_EQUALS.toString())
                    .build());
        }
        if (group.getComment() != null && !group.getComment().isEmpty()) {
            filterDto.setComment(StringFilter.builder()
                    .value(group.getComment())
                    .typeComparison(TEXT_EQUALS.toString())
                    .build());
        }
        return filterDto;
    }
}
