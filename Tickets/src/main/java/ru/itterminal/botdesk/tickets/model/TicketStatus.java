package ru.itterminal.botdesk.tickets.model;

import java.util.Objects;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.commons.model.BaseEntity;

@Entity
@Table(name = "ticket_statuses")
@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class TicketStatus extends BaseEntity {

    @ManyToOne
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @Column(nullable = false, length = 128)
    private String name;

    @Column(name = "sort_index", nullable = false)
    private Integer sortIndex;

    @Column(name = "is_started_predefined", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isStartedPredefined;

    @Column(name = "is_finished_predefined", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isFinishedPredefined;

    @Column(name = "is_reopened_predefined", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isReopenedPredefined;

    @Column(name = "is_canceled_predefined", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isCanceledPredefined;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        TicketStatus ticketStatus = (TicketStatus) o;
        return Objects.equals(name, ticketStatus.name) &&
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
        return Objects.hash(account, name, sortIndex, isStartedPredefined, isFinishedPredefined,
                            isReopenedPredefined, isCanceledPredefined,
                            getId(), getOutId(), getVersion(), getDeleted()
        );
    }
}
