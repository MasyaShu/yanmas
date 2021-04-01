package ru.itterminal.botdesk.tickets.model;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.commons.model.BaseEntity;

import javax.persistence.*;

@Entity
@Table(name = "ticket_templates")
@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class TicketTemplate extends BaseEntity {

    @ManyToOne(fetch = FetchType.LAZY)
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
    @JoinColumn(name = "author_id", nullable = false)
    private User author;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "ticket_type_id")
    private TicketType ticketType;

    @Override
    public void generateDisplayName() {
        setDisplayName(subject);
    }

    @PrePersist
    protected void onCreate() {
        setDeleted(false);
    }
}
