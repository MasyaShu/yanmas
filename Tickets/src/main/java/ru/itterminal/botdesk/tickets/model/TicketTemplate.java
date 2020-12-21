package ru.itterminal.botdesk.tickets.model;

import java.util.Objects;

import javax.persistence.*;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.commons.model.BaseEntity;

@Entity
@Table(name = "ticket_template")
@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
public class TicketTemplate extends BaseEntity {

    @ManyToOne
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

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

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "user_id", nullable = false)
    private User Author;

    @ManyToOne(fetch = FetchType.LAZY)
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
                Objects.equals(account, ticketTemplate.account) &&
                Objects.equals(Author, ticketTemplate.Author) &&
                Objects.equals(ticketType, ticketTemplate.ticketType) &&
                Objects.equals(getId(), ticketTemplate.getId()) &&
                Objects.equals(getOutId(), ticketTemplate.getOutId()) &&
                Objects.equals(getVersion(), ticketTemplate.getVersion()) &&
                Objects.equals(getDeleted(), ticketTemplate.getDeleted());
    }

    @Override
    public int hashCode() {
        return Objects.hash(account, subject, description, dateNextRun, dateStart, dateEnd, zoneId, expressionSchedule,
                isOnlyOneTicketInWork, isActive, Author, ticketType,
                getId(), getOutId(), getVersion(), getDeleted()
        );
    }

    @Override
    public void generateDisplayName() {
        setDisplayName(subject);
    }
}
