package ru.itterminal.yanmas.aau.service.business_handler.impl;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.model.test.GroupTestHelper;
import ru.itterminal.yanmas.aau.service.impl.GroupServiceImpl;
import ru.itterminal.yanmas.commons.model.spec.SpecificationsFactory;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static ru.itterminal.yanmas.commons.util.CommonConstants.SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS;

@SpringJUnitConfig(value = {GroupBusinessHandlerImpl.class})
@ActiveProfiles(SPRING_ACTIVE_PROFILE_FOR_UNIT_TESTS)
class GroupBusinessHandlerImplTest {

    @MockBean
    private GroupServiceImpl groupService;

    @MockBean
    private SpecificationsFactory specFactory;

    @Autowired
    GroupBusinessHandlerImpl groupBusinessHandler = new GroupBusinessHandlerImpl(groupService, specFactory);

    private final GroupTestHelper groupTestHelper = new GroupTestHelper();

    @Test
    void beforeUpdate_shouldSetIsInnerGroupFromDataBase() {
        var currentUser = new User();
        var group = groupTestHelper.getRandomValidEntity();
        var groupFromDataBase = groupTestHelper.getRandomValidEntity();
        group.setIsInner(!groupFromDataBase.getIsInner());
        when(groupService.findByIdAndAccountId(any(), any())).thenReturn(groupFromDataBase);
        groupBusinessHandler.beforeUpdate(group, currentUser);
        assertEquals(group.getIsInner(), groupFromDataBase.getIsInner());
    }

}