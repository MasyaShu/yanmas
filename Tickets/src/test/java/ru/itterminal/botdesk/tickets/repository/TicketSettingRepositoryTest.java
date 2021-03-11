package ru.itterminal.botdesk.tickets.repository;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.TestInstance.Lifecycle.PER_CLASS;

import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.jdbc.Sql;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import ru.itterminal.botdesk.tickets.model.TicketSetting;

@TestInstance(PER_CLASS)
@ExtendWith(SpringExtension.class)
@DataJpaTest
@ContextConfiguration(classes = {TicketRepositoryTestConfig.class, TicketSettingRepository.class})
@Sql({"/create-ticket-test.sql"})
class TicketSettingRepositoryTest {

    @Autowired
    private TicketSettingRepository repository;

    private static final UUID TICKET_SETTING_1_ID = UUID.fromString("9c8183ba-5d13-442f-a741-5b3134a3c140");
    private static final UUID ACCOUNT_1_ID = UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de");
    private static final UUID GROUP_1_ID = UUID.fromString("0223e51a-4bb2-44ee-bc8e-1f047a2145e7");
    private static final UUID USER_1_ID = UUID.fromString("d592facb-e6ee-4801-8310-9c7708eb6e6c");
    private static final UUID USER_2_ID = UUID.fromString("cdfa6483-0769-4628-ba32-efd338a716de");

    @Test
    void findAll_shouldFindOneRecordInDatabase_whenInDatabaseOnlyOne() {
        List<TicketSetting> ticketSettingsList = repository.findAllByAccountId(ACCOUNT_1_ID);
        assertFalse(ticketSettingsList.isEmpty());
    }

    @Test
    void findAllByAccount_IdAndGroup_IdAndAuthor_IdAndIdNot_shouldFindOneRecordInDatabase_whenCombinationOfFieldsIsNotUnique() {
        List<TicketSetting> ticketSettingUniqueFields =
                repository.findAllByAccount_IdAndGroup_IdAndAuthor_IdAndIdNot(
                        ACCOUNT_1_ID,
                        GROUP_1_ID,
                        USER_1_ID,
                        UUID.randomUUID()
                );
        assertFalse(ticketSettingUniqueFields.isEmpty());
    }

    @Test
    void findAllByAccount_IdAndGroup_IdAndAuthor_IdAndIdNot_shouldGetEmptyList_whenCombinationOfFieldsIsUnique() {
        List<TicketSetting> ticketSettingUniqueFields =
                repository.findAllByAccount_IdAndGroup_IdAndAuthor_IdAndIdNot(
                        ACCOUNT_1_ID,
                        GROUP_1_ID,
                        USER_2_ID,
                        UUID.randomUUID()
                );
        assertTrue(ticketSettingUniqueFields.isEmpty());
    }


    @Test
    void findAllByAccount_IdAndGroup_IdAndAuthor_IdAndIdNot_shouldGetEmptyList_whenCombinationOfFieldsIsUniqueForPassedId() {
        List<TicketSetting> ticketSettingUniqueFields =
                repository.findAllByAccount_IdAndGroup_IdAndAuthor_IdAndIdNot(
                        ACCOUNT_1_ID,
                        GROUP_1_ID,
                        USER_1_ID,
                        TICKET_SETTING_1_ID
                );
        assertTrue(ticketSettingUniqueFields.isEmpty());
    }

    @Test
    void findById_shouldFindOneTicketSetting_whenAccordingWithPlannedBehavior() {
        TicketSetting ticketSetting = repository.findById(TICKET_SETTING_1_ID).get();
        assertEquals(2, ticketSetting.getObservers().size());
        assertEquals(2, ticketSetting.getExecutors().size());
    }

    @Test
    void findById_shouldFindTwiceTheSameTicketSetting_whenAccordingWithPlannedBehavior() {
        TicketSetting ticketSetting1 = repository.findById(TICKET_SETTING_1_ID).get();
        TicketSetting ticketSetting2 = repository.findById(TICKET_SETTING_1_ID).get();
        assertEquals(ticketSetting1, ticketSetting2);
    }
}