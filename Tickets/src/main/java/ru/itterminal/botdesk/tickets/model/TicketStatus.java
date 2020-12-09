package ru.itterminal.botdesk.tickets.model;


import lombok.*;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.commons.model.BaseEntity;

import javax.persistence.*;
import java.util.Objects;

@Entity
@Table(name = "ticket_statuses")
@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TicketStatus extends BaseEntity {

    @Column(nullable = false, length = 128)
    private String name;

    @Column
    private String comment;

    @Column(name = "sort_index", nullable = false)
    private Short sortIndex; //TODO int or short

    @Column(name = "is_started_predefined", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isStartedPredefined;

    @Column(name = "is_finished_predefined", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isFinishedPredefined;

    @Column(name = "is_reopened_predefined", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isReopenedPredefined;

    @Column(name = "is_canceled_predefined", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isCanceledPredefined;

    @ManyToOne
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        TicketStatus ticketStatus = (TicketStatus) o;
        return Objects.equals(name, ticketStatus.name) &&
                Objects.equals(comment, ticketStatus.comment) &&
                Objects.equals(sortIndex, ticketStatus.sortIndex) &&
                Objects.equals(isStartedPredefined, ticketStatus.isStartedPredefined) &&
                Objects.equals(isFinishedPredefined, ticketStatus.isFinishedPredefined) &&
                Objects.equals(isReopenedPredefined, ticketStatus.isReopenedPredefined) &&
                Objects.equals(isCanceledPredefined, ticketStatus.isCanceledPredefined) &&
                Objects.equals(account, ticketStatus.account) &&
                Objects.equals(getId(), ticketStatus.getId()) &&
                Objects.equals(getOutId(), ticketStatus.getOutId()) &&
                Objects.equals(getVersion(), ticketStatus.getVersion()) &&
                Objects.equals(getDeleted(), ticketStatus.getDeleted());
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, comment, sortIndex, isStartedPredefined, isFinishedPredefined,
                isReopenedPredefined, isCanceledPredefined, account,
                getId(), getOutId(), getVersion(), getDeleted());
    }
}
