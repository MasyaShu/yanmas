package ru.itterminal.yanmas.tickets.model;

import java.util.List;
import java.util.UUID;

import javax.persistence.Column;
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
import ru.itterminal.yanmas.aau.model.Account;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.files.model.File;

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

    @JoinColumn(name = "ticket_id")
    private UUID ticketId;

    @Column
    private String comment;

    @Column(name = "auto_comment")
    private String autoComment;

    @Column(name = "created_at", nullable = false, updatable = false)
    private Long createdAt;

    @Column(name = "created_by", nullable = false, updatable = false)
    private Long createdBy;

    @SuppressWarnings("JpaDataSourceORMInspection")
    @OneToMany(fetch = FetchType.LAZY)
    @JoinColumn(name = "entity_id")
    private List<File> files;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "new_author_id")
    private User newAuthor;

    @Column(name = "new_subject")
    private String newSubject;

    @Column(name = "new_description")
    private String newDescription;

    @Column(name = "new_priority")
    private String newPriority;

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
            joinColumns = @JoinColumn(name = "ticket_event_id"),
            inverseJoinColumns = @JoinColumn(name = "observer_id")
    )
    private List<User> newObservers;

    @OneToMany(fetch = FetchType.LAZY)
    @JoinTable(
            name = "ticket_event_executors",
            joinColumns = @JoinColumn(name = "ticket_event_id"),
            inverseJoinColumns = @JoinColumn(name = "executor_id")
    )
    private List<User> newExecutors;

    @Override
    public void generateDisplayName() {
        setDisplayName(newSubject);
    }
}
