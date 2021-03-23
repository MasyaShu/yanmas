package ru.itterminal.botdesk.tickets.model;


import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.commons.model.BaseEntity;

import javax.persistence.*;
import java.util.List;

@Entity
@Table(name = "ticket_events")
@Getter
@Setter
@SuperBuilder(toBuilder = true)
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class TicketEvent extends BaseEntity {

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @Column
    private String comment;

    @Column(name = "is_auto_comment")
    private Boolean isAutoComment;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "ticket_id")
    private Ticket ticket;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "new_group_id")
    private Group newGroup;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "new_author_id")
    private User newAuthor;

    @Column(name = "created_at", nullable = false, updatable = false)
    private Long createdAt;

    @Column(name = "created_by", nullable = false, updatable = false)
    private Long createdBy;

    @Column(name = "new_subject")
    private String newSubject;

    @Column(name = "new_description")
    private String newDescription;

    @Column(name = "new_deadline")
    private Long newDeadline;

    @Column(name = "new_is_finished", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean newIsFinished;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "new_ticket_type_id")
    private TicketType newTicketType;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "new_ticket_status_id")
    private TicketStatus newTicketStatus;

    @OneToMany(fetch = FetchType.LAZY)
    @JoinTable(
            name = "ticket_event_observers",
            joinColumns = @JoinColumn(name = "ticket_id"),
            inverseJoinColumns = @JoinColumn(name = "observer_id")
    )
    private List<User> newObservers;

    @OneToMany(fetch = FetchType.LAZY)
    @JoinTable(
            name = "ticket_event_executors",
            joinColumns = @JoinColumn(name = "ticket_id"),
            inverseJoinColumns = @JoinColumn(name = "executor_id")
    )
    private List<User> newExecutors;

    @Override
    public void generateDisplayName() {
        setDisplayName(newSubject);
    }
}
