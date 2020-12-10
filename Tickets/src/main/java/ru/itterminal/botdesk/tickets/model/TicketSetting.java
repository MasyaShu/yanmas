package ru.itterminal.botdesk.tickets.model;

import java.util.Objects;

import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.commons.model.BaseEntity;

@Entity
@Table(name = "ticket_settings")
@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TicketSetting extends BaseEntity {

    @ManyToOne
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @ManyToOne
    @JoinColumn(name = "author_id")
    private User author;

    @ManyToOne
    @JoinColumn(name = "ticket_type_id_for_new")
    private TicketType ticketTypeForNew;

    @ManyToOne
    @JoinColumn(name = "ticket_status_id_for_new")
    private TicketStatus ticketStatusForNew;

    @ManyToOne
    @JoinColumn(name = "ticket_status_id_for_reopen")
    private TicketStatus ticketStatusForReopen;

    @ManyToOne
    @JoinColumn(name = "ticket_status_id_for_close")
    private TicketStatus ticketStatusForClose;

    @ManyToOne
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
        return Objects
                .hash(account, author, ticketTypeForNew, ticketStatusForNew, ticketStatusForReopen,
                      ticketStatusForClose,
                      ticketStatusForCancel,
                      getId(), getOutId(), getVersion(), getDeleted()
                );
    }

}
