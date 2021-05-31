package ru.itterminal.yanmas.aau.model.test;

import static ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.EXIST_IN;
import static ru.itterminal.yanmas.commons.model.filter.StringFilter.TypeComparisonForStringFilter.TEXT_EQUALS;

import java.util.List;
import java.util.UUID;

import org.modelmapper.ModelMapper;

import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.model.dto.AuthenticationRequestDto;
import ru.itterminal.yanmas.aau.model.dto.UserDtoRequest;
import ru.itterminal.yanmas.aau.model.dto.UserDtoResponse;
import ru.itterminal.yanmas.aau.model.dto.UserFilterDto;
import ru.itterminal.yanmas.commons.model.EntityTestHelperImpl;
import ru.itterminal.yanmas.commons.model.filter.BaseEntityFilter;
import ru.itterminal.yanmas.commons.model.filter.BooleanFilter;
import ru.itterminal.yanmas.commons.model.filter.StringFilter;

@SuppressWarnings("DuplicatedCode")
public class UserTestHelper extends EntityTestHelperImpl<User, UserDtoRequest, UserDtoResponse> {

    private final RoleTestHelper roleHelper = new RoleTestHelper();
    private final AccountTestHelper accountHelper = new AccountTestHelper();
    private final GroupTestHelper groupHelper = new GroupTestHelper();
    private final ModelMapper modelMapper = new ModelMapper();

    private static final String PASSWORD = "$2a$10$ejjUoWxoPs.4vjQOoBnFquybNHJAxIyKz7QIXL6.BSVyHPow8jmxK";
    private static final String PHONE = "211-15-15";
    private static final String COMMENT = "comment comment";

    @Override
    public User getRandomValidEntity() {
        User user = User.builder()
                .email(fakerEN.bothify("???????##@yanmas.app"))
                .name(fakerRU.name().firstName())
                .password(fakerRU.bothify("????##???##F"))
                .phone(fakerRU.phoneNumber().phoneNumber())
                .comment(fakerRU.lorem().paragraph())
                .emailVerificationStatus(fakerRU.bool().bool())
                .isArchived(fakerRU.bool().bool())
                .account(accountHelper.getRandomValidEntity())
                .group(groupHelper.getRandomValidEntity())
                .role(roleHelper.getRandomValidEntity())
                .build();
        setRandomValidPropertiesOfBaseEntity(user);
        return user;
    }

    @Override
    public List<User> setPredefinedValidEntityList() {

        User user1 = User.builder()
                .email("m@m.ru")
                .name("Name1")
                .password(PASSWORD)
                .phone(PHONE)
                .comment(COMMENT)
                .emailVerificationToken(null)
                .emailVerificationStatus(true)
                .passwordResetToken(null)
                .isArchived(false)
                .account(accountHelper.getPredefinedValidEntityList().get(0))
                .group(groupHelper.getPredefinedValidEntityList().get(0))
                .role(roleHelper.getPredefinedValidEntityList().get(0))
                .build();
        setPropertiesOfBaseEntity(
                user1,
                UUID.fromString("d592facb-e6ee-4801-8310-9c7708eb6e6c"),
                0,
                false,
                null
        );
        User user2 = User.builder()
                .email("m1@m.ru")
                .name("Name2")
                .password(PASSWORD)
                .phone(PHONE)
                .comment(COMMENT)
                .emailVerificationToken(null)
                .emailVerificationStatus(false)
                .passwordResetToken(null)
                .isArchived(false)
                .account(accountHelper.getPredefinedValidEntityList().get(0))
                .group(groupHelper.getPredefinedValidEntityList().get(0))
                .role(roleHelper.getPredefinedValidEntityList().get(3))
                .build();
        setPropertiesOfBaseEntity(
                user2,
                UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de"),
                0,
                false,
                null
        );
        User user3 = User.builder()
                .email("m2@m.ru")
                .name(null)
                .password(PASSWORD)
                .phone(null)
                .comment(null)
                .emailVerificationToken(null)
                .emailVerificationStatus(false)
                .passwordResetToken(null)
                .isArchived(false)
                .account(accountHelper.getPredefinedValidEntityList().get(0))
                .group(groupHelper.getPredefinedValidEntityList().get(0))
                .role(roleHelper.getPredefinedValidEntityList().get(3))
                .build();
        setPropertiesOfBaseEntity(
                user3,
                UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7"),
                0,
                false,
                "outId468431"
        );
        User user4 = User.builder()
                .email("m3@m.ru")
                .name("")
                .password(PASSWORD)
                .phone("")
                .comment("")
                .emailVerificationToken(null)
                .emailVerificationStatus(false)
                .passwordResetToken(null)
                .isArchived(false)
                .account(accountHelper.getPredefinedValidEntityList().get(0))
                .group(groupHelper.getPredefinedValidEntityList().get(0))
                .role(roleHelper.getPredefinedValidEntityList().get(3))
                .build();
        setPropertiesOfBaseEntity(
                user4,
                UUID.fromString("e14d9ffd-0071-4c0e-99ed-932f007963f0"),
                0,
                false,
                ""
        );
        User user5 = User.builder()
                .email("m4@m.ru")
                .name("Name5")
                .password(PASSWORD)
                .phone(PHONE)
                .comment(COMMENT)
                .emailVerificationToken(null)
                .emailVerificationStatus(false)
                .passwordResetToken(null)
                .isArchived(false)
                .account(accountHelper.getPredefinedValidEntityList().get(1))
                .group(groupHelper.getPredefinedValidEntityList().get(2))
                .role(roleHelper.getPredefinedValidEntityList().get(3))
                .build();
        setPropertiesOfBaseEntity(
                user5,
                UUID.fromString("86840939-c488-448b-a473-cd9e1097dd32"),
                0,
                false,
                null
        );
        return List.of(user1, user2, user3, user4, user5);
    }

    public AuthenticationRequestDto convertUserToAuthenticationRequestDto(User user) {
        return modelMapper.map(user, AuthenticationRequestDto.class);
    }

    public UserDtoRequest convertUserToUserDtoRequest(User createdUser, boolean isCreated) {
        var userDtoRequest = modelMapper.map(createdUser, UserDtoRequest.class);
        userDtoRequest.setGroupId(createdUser.getGroup().getId());
        userDtoRequest.setRoleId(createdUser.getRole().getId());
        userDtoRequest.setDisplayName(null);
        if (isCreated) {
            userDtoRequest.setId(null);
            userDtoRequest.setDeleted(null);
            userDtoRequest.setVersion(null);
            userDtoRequest.setIsArchived(null);
        }
        return userDtoRequest;

    }

    public UserFilterDto convertEntityToFilterDto(User user) {
        var filterDto = UserFilterDto.builder()
                .email(StringFilter.builder()
                               .typeComparison(TEXT_EQUALS.toString())
                               .value(user.getEmail())
                               .build())
                .group(BaseEntityFilter.builder()
                               .typeComparison(EXIST_IN.toString())
                               .listOfIdEntities(List.of(user.getGroup().getId()))
                               .build())
                .role(BaseEntityFilter.builder()
                               .typeComparison(EXIST_IN.toString())
                               .listOfIdEntities(List.of(user.getRole().getId()))
                               .build())
                .isArchived(BooleanFilter.builder()
                                    .value(user.getIsArchived())
                                    .build())
                .deleted(BooleanFilter.builder()
                                 .value(user.getDeleted())
                                 .build())
                .build();

        if (user.getOutId() != null && !user.getOutId().isEmpty()) {
            filterDto.setOutId(StringFilter.builder()
                                       .value(user.getOutId())
                                       .typeComparison(TEXT_EQUALS.toString())
                                       .build());
        }
        if (user.getName() != null && !user.getName().isEmpty()) {
            filterDto.setName(StringFilter.builder()
                                         .value(user.getName())
                                         .typeComparison(TEXT_EQUALS.toString())
                                         .build());
        }
        if (user.getPhone() != null && !user.getPhone().isEmpty()) {
            filterDto.setPhone(StringFilter.builder()
                                         .value(user.getPhone())
                                         .typeComparison(TEXT_EQUALS.toString())
                                         .build());
        }
        if (user.getComment() != null && !user.getComment().isEmpty()) {
            filterDto.setComment(StringFilter.builder()
                                         .value(user.getComment())
                                         .typeComparison(TEXT_EQUALS.toString())
                                         .build());
        }
        return filterDto;
    }
}
