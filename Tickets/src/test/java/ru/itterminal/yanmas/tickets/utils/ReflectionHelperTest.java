package ru.itterminal.yanmas.tickets.utils;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.yanmas.aau.model.test.UserTestHelper;
import ru.itterminal.yanmas.aau.service.impl.AccountServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.GroupServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.RoleServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.aau.util.ReflectionHelper;
import ru.itterminal.yanmas.security.jwt.JwtUser;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.model.test.TicketTestHelper;

@SpringJUnitConfig(value = {ReflectionHelper.class, ModelMapper.class, JwtUserBuilder.class, UserServiceImpl.class,
        AccountServiceImpl.class, GroupServiceImpl.class, RoleServiceImpl.class})
class ReflectionHelperTest {

    @Autowired
    private ReflectionHelper reflectionHelper;

    @MockBean
    private JwtUserBuilder jwtUserBuilder;

    @SuppressWarnings("unused")
    @MockBean
    private UserServiceImpl userService;

    @SuppressWarnings("unused")
    @MockBean
    private AccountServiceImpl accountService;

    @SuppressWarnings("unused")
    @MockBean
    private GroupServiceImpl groupService;

    @SuppressWarnings("unused")
    @MockBean
    private RoleServiceImpl roleService;

    private final TicketTestHelper ticketTestHelper = new TicketTestHelper();
    private final UserTestHelper userTestHelper = new UserTestHelper();

    @Test
    void convertRequestDtoIntoEntityWithNestedObjectsWithOnlyValidId_shouldGetValidEntity_whenPassedValidDtoObject() {
        var expectedTicket = ticketTestHelper.getRandomValidEntity();
        when(jwtUserBuilder.getJwtUser()).thenReturn
                (JwtUser.builder()
                         .accountId(expectedTicket.getAccount().getId())
                         .build()
                );
        var ticketDtoRequest = ticketTestHelper.convertEntityToDtoRequest(expectedTicket, true);
        var actualTicket = (Ticket) reflectionHelper
                .convertRequestDtoIntoEntityWhereNestedObjectsWithOnlyValidId(ticketDtoRequest, Ticket.class);
        assertThat(actualTicket).usingRecursiveComparison().ignoringActualNullFields()
                .isEqualTo(expectedTicket);
    }

    @Test
    void settingNestedObjectsIntoEntity_shouldSetAllNestedObjects () {
        var testUser = userTestHelper.getRandomValidEntity();
        var currentUser = userTestHelper.getRandomValidEntity();
        reflectionHelper.settingNestedObjectsIntoEntity(testUser, currentUser);
    }

}
