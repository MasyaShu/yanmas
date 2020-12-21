package ru.itterminal.botdesk.aau.model.test;

import java.util.List;
import java.util.UUID;

import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.dto.UserDtoRequest;
import ru.itterminal.botdesk.aau.model.dto.UserDtoResponse;
import ru.itterminal.botdesk.commons.model.EntityTestHelperImpl;

public class UserTestHelper extends EntityTestHelperImpl<User, UserDtoRequest, UserDtoResponse> {

    RoleTestHelper roleHelper = new RoleTestHelper();
    AccountTestHelper accountHelper = new AccountTestHelper();
    GroupTestHelper groupHelper = new GroupTestHelper();

    private static final String PASSWORD = "$2a$10$ejjUoWxoPs.4vjQOoBnFquybNHJAxIyKz7QIXL6.BSVyHPow8jmxK";
    private static final String PHONE = "211-15-15";
    private static final String COMMENT = "comment comment";

    @Override
    public User getRandomValidEntity() {
        User user = User.builder()
                .email(fakerEN.bothify("???????##@botdesk.app"))
                .name(fakerRU.name().firstName())
                .password(fakerRU.bothify("????##???##"))
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
}
