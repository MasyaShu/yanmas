package ru.itterminal.botdesk.tickets.model;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.commons.model.BaseEntity;

@Entity
@Table(name = "ticket_settings")
@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class TicketSetting extends BaseEntity {

    @ManyToOne (fetch = FetchType.LAZY)
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "group_id")
    private Group group;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "author_id")
    private User author;

    @OneToMany(fetch = FetchType.LAZY)
    @JoinTable(
            name = "ticket_settings_observers",
            joinColumns = @JoinColumn(name = "ticket_settings_id"),
            inverseJoinColumns = @JoinColumn(name = "observer_id")
    )
    List<User> observers = new ArrayList<>();

    @OneToMany(fetch = FetchType.LAZY)
    @JoinTable(
            name = "ticket_settings_executors",
            joinColumns = @JoinColumn(name = "ticket_settings_id"),
            inverseJoinColumns = @JoinColumn(name = "executor_id")
    )
    List<User> executors = new ArrayList<>();

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "ticket_type_id_for_new")
    private TicketType ticketTypeForNew;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "ticket_status_id_for_new")
    private TicketStatus ticketStatusForNew;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "ticket_status_id_for_reopen")
    private TicketStatus ticketStatusForReopen;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "ticket_status_id_for_close")
    private TicketStatus ticketStatusForClose;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "ticket_status_id_for_cancel")
    private TicketStatus ticketStatusForCancel;

    @Override
    public void generateDisplayName() {

        String resultGroup = getGroup() != null && getGroup().getDisplayName() != null
                ? getGroup().getDisplayName()
                : null;
        String resultAuthor = getAuthor() != null && getAuthor().getDisplayName() != null
                ? getAuthor().getDisplayName()
                : null;

        if (resultGroup != null && resultAuthor != null) {
            setDisplayName(resultGroup + " " + resultAuthor);
        }

        if (resultGroup != null && resultAuthor == null) {
            setDisplayName(resultGroup);
        }

        if (resultGroup == null && resultAuthor != null) {
            setDisplayName(resultAuthor);
        }

    }
}
