package ru.itterminal.yanmas.aau.service.validator.group;

import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Import;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.test.context.support.WithUserDetails;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.yanmas.aau.model.test.GroupTestHelper;
import ru.itterminal.yanmas.aau.model.test.RoleTestHelper;
import ru.itterminal.yanmas.aau.model.test.UserTestHelper;
import ru.itterminal.yanmas.aau.service.validator.group.check_access_before_read.ReadDeniedIfCurrentUserInOuterGroupAndCurrentUserGroupIsNotEqualEntityValidator;
import ru.itterminal.yanmas.security.config.TestSecurityConfig;

import java.util.stream.Stream;

import static org.assertj.core.api.AssertionsForClassTypes.assertThat;
import static org.assertj.core.api.AssertionsForClassTypes.catchThrowable;
import static org.junit.jupiter.api.Assertions.*;
import static ru.itterminal.yanmas.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;
import static ru.itterminal.yanmas.security.config.TestSecurityConfig.*;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringJUnitConfig(value = {ReadDeniedIfCurrentUserInOuterGroupAndCurrentUserGroupIsNotEqualEntityValidator.class})
@Import(TestSecurityConfig.class)
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class ReadDeniedIfCurrentUserInOuterGroupAndCurrentUserGroupIsNotEqualEntityValidatorTest {

    @Autowired
    ReadDeniedIfCurrentUserInOuterGroupAndCurrentUserGroupIsNotEqualEntityValidator readDeniedIfCurrentUserInOuterGroupAndCurrentUserGroupIsNotEqualEntityValidator;
    private final GroupTestHelper groupTestHelper = new GroupTestHelper();
    private final UserTestHelper userTestHelper = new UserTestHelper();
    private final RoleTestHelper roleTestHelper = new RoleTestHelper();

    @ParameterizedTest(name = "{index} User role: {0} isInner: {1}")
    @MethodSource("getUserWhoCanReadAllGroups")
    @WithUserDetails("EXECUTOR_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void checkAccessBeforeRead_shouldGetNoError_whenCurrentUserByInnerGroup(String role, Boolean isInnerGroup) {
        var groupForRead = groupTestHelper.getRandomValidEntity();
        var currentUser = userTestHelper.getRandomValidEntity();
        currentUser.setRole(roleTestHelper.getRoleByName(role));
        currentUser.getGroup().setIsInner(isInnerGroup);
        Throwable throwable = catchThrowable(() ->
                readDeniedIfCurrentUserInOuterGroupAndCurrentUserGroupIsNotEqualEntityValidator
                        .checkAccessBeforeRead(groupForRead, currentUser));
        assertThat(throwable).isNull();
    }

    @ParameterizedTest(name = "{index} User role: {0} isInner: {1}")
    @MethodSource("getUsersWhoCanReadOnlyTheirGroup")
    @WithUserDetails("EXECUTOR_ACCOUNT_1_IS_NOT_INNER_GROUP")
    void checkAccessBeforeRead_shouldGetAccessDeniedException_whenCurrentUserByOuterGroup(String role, Boolean isInnerGroup) {
        var groupForRead = groupTestHelper.getRandomValidEntity();
        var currentUser = userTestHelper.getRandomValidEntity();
        currentUser.setRole(roleTestHelper.getRoleByName(role));
        currentUser.getGroup().setIsInner(isInnerGroup);
        assertThrows(AccessDeniedException.class,
                () -> readDeniedIfCurrentUserInOuterGroupAndCurrentUserGroupIsNotEqualEntityValidator
                        .checkAccessBeforeRead(groupForRead, currentUser));
    }

    private Stream<Arguments> getUserWhoCanReadAllGroups() {
        return Stream.of(
                Arguments.of(ROLE_OBSERVER, true),
                Arguments.of(ROLE_AUTHOR, true),
                Arguments.of(ROLE_EXECUTOR, true),
                Arguments.of(ROLE_ADMIN, true),
                Arguments.of(ROLE_ACCOUNT_OWNER, true)
        );
    }

    private Stream<Arguments> getUsersWhoCanReadOnlyTheirGroup() {
        return Stream.of(
                Arguments.of(ROLE_OBSERVER, false),
                Arguments.of(ROLE_AUTHOR, false),
                Arguments.of(ROLE_EXECUTOR, false),
                Arguments.of(ROLE_ADMIN, false)
        );
    }

}