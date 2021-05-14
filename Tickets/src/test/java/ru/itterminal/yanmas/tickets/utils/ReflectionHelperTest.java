package ru.itterminal.yanmas.tickets.utils;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

import org.hibernate.validator.constraints.URL;
import org.junit.jupiter.api.Test;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.yanmas.aau.model.Account;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.Role;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.aau.model.test.UserTestHelper;
import ru.itterminal.yanmas.aau.repository.AccountRepository;
import ru.itterminal.yanmas.aau.repository.GroupRepository;
import ru.itterminal.yanmas.aau.repository.RoleRepository;
import ru.itterminal.yanmas.aau.repository.UserRepository;
import ru.itterminal.yanmas.aau.service.impl.AccountServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.GroupServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.RoleServiceImpl;
import ru.itterminal.yanmas.aau.service.impl.UserServiceImpl;
import ru.itterminal.yanmas.aau.util.ReflectionHelper;
import ru.itterminal.yanmas.security.jwt.JwtUser;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.model.test.TicketTestHelper;

import java.util.Optional;

@SpringJUnitConfig(value = {ReflectionHelper.class, ModelMapper.class, JwtUserBuilder.class, UserRepository.class,
        AccountRepository.class, GroupRepository.class, RoleRepository.class})
class ReflectionHelperTest {

    @Autowired
    private ReflectionHelper reflectionHelper;

    @MockBean
    private JwtUserBuilder jwtUserBuilder;

    @SuppressWarnings("unused")
    @MockBean
    private UserRepository userRepository;

    @SuppressWarnings("unused")
    @MockBean
    private AccountRepository accountRepository;

    @SuppressWarnings("unused")
    @MockBean
    private GroupRepository groupRepository;

    @SuppressWarnings("unused")
    @MockBean
    private RoleRepository roleRepository;

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
        when(userRepository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(new User()));
        when(accountRepository.findById(any())).thenReturn(Optional.of(new Account()));
        when(groupRepository.findByIdAndAccountId(any(), any())).thenReturn(Optional.of(new Group()));
        when(roleRepository.findById(any())).thenReturn(Optional.of(new Role()));
        var testUser = userTestHelper.getRandomValidEntity();
        var currentUser = userTestHelper.getRandomValidEntity();
        reflectionHelper.settingNestedObjectsIntoEntity(testUser, currentUser);
    }

}
