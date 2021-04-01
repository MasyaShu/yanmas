package ru.itterminal.yanmas.tickets.utils;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.modelmapper.ModelMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import ru.itterminal.yanmas.aau.util.ReflectionHelper;
import ru.itterminal.yanmas.security.jwt.JwtUser;
import ru.itterminal.yanmas.security.jwt.JwtUserBuilder;
import ru.itterminal.yanmas.tickets.model.Ticket;
import ru.itterminal.yanmas.tickets.model.test.TicketTestHelper;

@SpringJUnitConfig(value = {ReflectionHelper.class, ModelMapper.class, JwtUserBuilder.class})
class ReflectionHelperTest {

    @Autowired
    private ReflectionHelper reflectionHelper;

    @MockBean
    private JwtUserBuilder jwtUserBuilder;

    private final TicketTestHelper ticketTestHelper = new TicketTestHelper();

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

}
