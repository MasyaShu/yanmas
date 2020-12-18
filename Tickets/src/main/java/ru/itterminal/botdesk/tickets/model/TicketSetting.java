package ru.itterminal.botdesk.tickets.model;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
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
public class TicketSetting extends BaseEntity {

    @ManyToOne
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
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof TicketSetting)) {
            return false;
        }
        TicketSetting that = (TicketSetting) o;
        return Objects.equals(account, that.account) &&
                Objects.equals(author, that.author) &&
                Objects.equals(group, that.group) &&
                Objects.equals(observers, that.observers) &&
                Objects.equals(executors, that.executors) &&
                Objects.equals(ticketTypeForNew, that.ticketTypeForNew) &&
                Objects.equals(ticketStatusForNew, that.ticketStatusForNew) &&
                Objects.equals(ticketStatusForReopen, that.ticketStatusForReopen) &&
                Objects.equals(ticketStatusForClose, that.ticketStatusForClose) &&
                Objects.equals(ticketStatusForCancel, that.ticketStatusForCancel) &&
                Objects.equals(getId(), that.getId()) &&
                Objects.equals(getOutId(), that.getOutId()) &&
                Objects.equals(getVersion(), that.getVersion()) &&
                Objects.equals(getDeleted(), that.getDeleted());

    }

    @Override
    public int hashCode() {
        return Objects.hash(account, author, group, observers, executors, ticketTypeForNew, ticketStatusForNew,
                            ticketStatusForReopen, ticketStatusForClose, ticketStatusForCancel,
                            getId(), getOutId(), getVersion(), getDeleted()
        );
    }

    @Override
    public void generateDisplayName() {
        String result = getGroup().getDisplayName() != null ? getGroup().getDisplayName() : null;
        if (result != null && getAuthor().getDisplayName() != null) {
            result = result + " ";
        }
        result = getAuthor() != null ? getAuthor().getDisplayName() : null;
        setDisplayName(result);
    }
}
