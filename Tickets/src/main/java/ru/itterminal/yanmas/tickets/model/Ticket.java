package ru.itterminal.yanmas.tickets.model;

import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.JoinTable;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.PrePersist;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.aau.model.Account;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.files.model.File;

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

    @SuppressWarnings({"RedundantIfStatement", "EqualsReplaceableByObjectsCall"})
    public boolean equalsBeforeUpdate(Object o) {
        Ticket ticket = (Ticket) o;
        if (account != null ? !account.equals(ticket.account) : ticket.account != null) { //NOSONAR
            return false;
        }
        if (author != null ? !author.equals(ticket.author) : ticket.author != null) { //NOSONAR
            return false;
        }
        if (subject != null ? !subject.equals(ticket.subject) : ticket.subject != null) { //NOSONAR
            return false;
        }
        if (priority != null ? !priority.equals(ticket.priority) : ticket.priority != null) { //NOSONAR
            return false;
        }
        if (description != null ? !description.equals(ticket.description) : ticket.description != null) { //NOSONAR
            return false;
        }
        if (deadline != null ? !deadline.equals(ticket.deadline) : ticket.deadline != null) { //NOSONAR
            return false;
        }
        if (isFinished != null ? !isFinished.equals(ticket.isFinished) : ticket.isFinished != null) { //NOSONAR
            return false;
        }
        if (ticketType != null ? !ticketType.equals(ticket.ticketType) : ticket.ticketType != null) { //NOSONAR
            return false;
        }
        if (ticketStatus != null ? !ticketStatus.equals(ticket.ticketStatus) : ticket.ticketStatus != null) { //NOSONAR
            return false;
        }
        if (observers != null ? !observers.equals(ticket.observers) : ticket.observers != null) { //NOSONAR
            return false;
        }
        if (executors != null ? !executors.equals(ticket.executors) : ticket.executors != null) { //NOSONAR
            return false;
        }
        if (getOutId() != null ? !getOutId().equals(ticket.getOutId()) : ticket.getOutId() != null) {
            return false;
        }
        if (getDeleted() != null ? !getDeleted().equals(ticket.getDeleted()) : ticket.getDeleted() != null) { //NOSONAR
            return false;
        }
        return true;
    }


}
