package ru.itterminal.botdesk.tickets.model.projection;

import ru.itterminal.botdesk.commons.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.User;

public interface TicketSettingUniqueFields {
    Account getAccount();
    User getAuthor();
    Group getGroup();
}
