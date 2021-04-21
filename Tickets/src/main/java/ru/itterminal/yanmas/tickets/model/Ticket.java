package ru.itterminal.yanmas.tickets.model;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.aau.model.Account;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.files.model.File;

import javax.persistence.*;
import java.util.List;

@Entity
@Table(name = "tickets")
@Getter
@Setter
@SuperBuilder(toBuilder = true)
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class Ticket extends BaseEntity {

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "group_id")
    private Group group;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "author_id")
    private User author;

    @Column(nullable = false, updatable = false)
    private Long number;

    @Column(name = "created_at", nullable = false, updatable = false)
    private Long createdAt;

    @Column
    private String subject;

    @Column
    private String priority;

    @Column
    private String description;

    @Column
    private Long deadline;

    @Column(name = "is_finished", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isFinished;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "ticket_type_id")
    private TicketType ticketType;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "ticket_status_id")
    private TicketStatus ticketStatus;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "ticket_template_id")
    private TicketTemplate ticketTemplate;

    @OneToMany(fetch = FetchType.LAZY)
    @JoinTable(
            name = "ticket_observers",
            joinColumns = @JoinColumn(name = "ticket_id"),
            inverseJoinColumns = @JoinColumn(name = "observer_id")
    )
    private List<User> observers;

    @OneToMany(fetch = FetchType.LAZY)
    @JoinTable(
            name = "ticket_executors",
            joinColumns = @JoinColumn(name = "ticket_id"),
            inverseJoinColumns = @JoinColumn(name = "executor_id")
    )
    private List<User> executors;

    @SuppressWarnings("JpaDataSourceORMInspection")
    @OneToMany(fetch = FetchType.LAZY)
    @JoinColumn(name = "entity_id")
    private List<File> files;

    @PrePersist
    protected void onCreate() {
        createdAt = System.currentTimeMillis();
        isFinished = false;
        setDeleted(false);
    }

    @Override
    public void generateDisplayName() {
        var result = number.toString();
        if (!subject.isEmpty()) {
            result = result + " " + subject;
        }
        setDisplayName(result);
    }
}