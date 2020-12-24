package ru.itterminal.botdesk.tickets.model.projection;

import java.util.UUID;

import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.User;

public interface TicketSettingUniqueFields {
    UUID getId();
    Account getAccount();
    User getAuthor();
    Group getGroup();
}
