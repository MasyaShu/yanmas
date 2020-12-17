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
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.commons.model.BaseEntityWithAccount;

@Entity
@Table(name = "ticket_template")
@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class TicketTemplate extends BaseEntityWithAccount {

    @Column(length = 256)
    private String subject;

    @Column
    private String description;

    @Column(name = "date_next_run")
    private Long dateNextRun;

    @Column(name = "date_start")
    private Long dateStart;

    @Column(name = "date_end")
    private Long dateEnd;

    @Column(name = "zone_id", nullable = false)
    private String zoneId;

    @Column(name = "expression_schedule", nullable = false)
    private String expressionSchedule;

    @Column(name = "is_only_one_ticket_in_work", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isOnlyOneTicketInWork;

    @Column(name = "is_active", nullable = false, columnDefinition = "BOOLEAN DEFAULT TRUE")
    private Boolean isActive;

    @ManyToOne
    @JoinColumn(name = "user_id", nullable = false)
    private User Author;

    @ManyToOne
    @JoinColumn(name = "type_ticket_id", nullable = false)
    private TicketType ticketType;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        TicketTemplate ticketTemplate = (TicketTemplate) o;
        return Objects.equals(subject, ticketTemplate.subject) &&
                Objects.equals(description, ticketTemplate.description) &&
                Objects.equals(dateNextRun, ticketTemplate.dateNextRun) &&
                Objects.equals(dateStart, ticketTemplate.dateStart) &&
                Objects.equals(dateEnd, ticketTemplate.dateEnd) &&
                Objects.equals(zoneId, ticketTemplate.zoneId) &&
                Objects.equals(expressionSchedule, ticketTemplate.expressionSchedule) &&
                Objects.equals(isOnlyOneTicketInWork, ticketTemplate.isOnlyOneTicketInWork) &&
                Objects.equals(isActive, ticketTemplate.isActive) &&
                Objects.equals(getAccount(), ticketTemplate.getAccount()) &&
                Objects.equals(Author, ticketTemplate.Author) &&
                Objects.equals(ticketType, ticketTemplate.ticketType) &&
                Objects.equals(getId(), ticketTemplate.getId()) &&
                Objects.equals(getOutId(), ticketTemplate.getOutId()) &&
                Objects.equals(getVersion(), ticketTemplate.getVersion()) &&
                Objects.equals(getDeleted(), ticketTemplate.getDeleted());
    }

    @Override
    public int hashCode() {
        return Objects.hash(subject, description, dateNextRun, dateStart, dateEnd, zoneId, expressionSchedule,
                            isOnlyOneTicketInWork, isActive, Author, ticketType,
                            getId(), getOutId(), getVersion(), getDeleted(), getAccount()
        );
    }
}