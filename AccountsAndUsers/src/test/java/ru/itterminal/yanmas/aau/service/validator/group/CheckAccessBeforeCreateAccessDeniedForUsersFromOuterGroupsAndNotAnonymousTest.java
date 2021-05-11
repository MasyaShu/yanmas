package ru.itterminal.yanmas.aau.service.validator.group;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Import;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.test.context.support.WithAnonymousUser;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.model.test.GroupTestHelper;
import ru.itterminal.yanmas.security.config.TestSecurityConfig;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.assertj.core.api.AssertionsForClassTypes.catchThrowable;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static ru.itterminal.yanmas.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;

@SpringJUnitConfig(value = {CheckAccessBeforeCreateAccessDeniedForUsersFromOuterGroupsAndNotAnonymous.class})
@Import(TestSecurityConfig.class)
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class CheckAccessBeforeCreateAccessDeniedForUsersFromOuterGroupsAndNotAnonymousTest {

    @Autowired
    CheckAccessBeforeCreateAccessDeniedForUsersFromOuterGroupsAndNotAnonymous checkAccessBeforeCreateAccessDeniedForUsersFromOuterGroupsAndNotAnonymous;
    private final GroupTestHelper groupTestHelper = new GroupTestHelper();

    @Test
    @WithAnonymousUser
    void checkAccessBeforeCreate_shouldGetNoError_whenAnonymousUser() {
        Group group = groupTestHelper.getRandomValidEntity();
        var currentUser = User.builder().group(group).build();
        Throwable throwable = catchThrowable(() -> checkAccessBeforeCreateAccessDeniedForUsersFromOuterGroupsAndNotAnonymous
                .checkAccessBeforeCreate(currentUser));
        assertThat(throwable).isNull();
    }

    @Test
    @WithUserDetails("EXECUTOR_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void checkAccessBeforeCreate_shouldGetNoError_whenCurrentUserByInnerGroup() {
        Group group = groupTestHelper.getRandomValidEntity();
        group.setIsInner(true);
        var currentUser = User.builder().group(group).build();
        Throwable throwable = catchThrowable(() -> checkAccessBeforeCreateAccessDeniedForUsersFromOuterGroupsAndNotAnonymous
                .checkAccessBeforeCreate(currentUser));
        assertThat(throwable).isNull();
    }

    @Test
    @WithUserDetails("EXECUTOR_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void checkAccessBeforeCreate_shouldGetAccessDeniedException_whenCurrentUserByOuterGroup() {
        Group group = groupTestHelper.getRandomValidEntity();
        group.setIsInner(false);
        var currentUser = User.builder().group(group).build();
        assertThrows(AccessDeniedException.class,
                () -> checkAccessBeforeCreateAccessDeniedForUsersFromOuterGroupsAndNotAnonymous
                        .checkAccessBeforeCreate(currentUser));
    }

}