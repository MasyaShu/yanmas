package ru.itterminal.botdesk.tickets.model.spec;

import org.springframework.stereotype.Component;

import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.commons.model.spec.BaseSpec;
import ru.itterminal.botdesk.tickets.model.TicketSetting;

@Component
public class TicketSettingSpec implements BaseSpec<TicketSetting, Account> {
}
