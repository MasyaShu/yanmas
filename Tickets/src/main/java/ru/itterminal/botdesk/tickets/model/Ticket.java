package ru.itterminal.botdesk.tickets.model;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

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
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.aau.model.Group;
import ru.itterminal.botdesk.aau.model.User;
import ru.itterminal.botdesk.commons.model.BaseEntity;
import ru.itterminal.botdesk.files.model.File;
import ru.itterminal.botdesk.tickets.model.dto.TicketDtoRequest;

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

    public static Ticket convertRequestDtoIntoEntityWithNestedObjectsWithOnlyId(TicketDtoRequest request,
                                                                                UUID accountId) {
        var ticket = Ticket.builder()
                .account(Account.builder().id(accountId).build())
                .author(request.getAuthor() == null ? null : User.builder().id(request.getAuthor()).build())
                .subject(request.getSubject() == null ? null : request.getSubject())
                .description(request.getDescription() == null ? null : request.getDescription())
                .deadline(request.getDeadline() == null ? null : request.getDeadline())
                .isFinished(request.getIsFinished() == null ? null : request.getIsFinished())
                .ticketType(request.getTicketType() == null ? null
                                    : TicketType.builder().id(request.getTicketType()).build())
                .ticketStatus(request.getTicketStatus() == null ? null
                                      : TicketStatus.builder().id(request.getTicketStatus()).build())
                .observers(request.getObservers() == null ? null
                                   : request.getObservers().stream()
                                           .map(id -> User.builder().id(id).build())
                                           .collect(Collectors.toList()))
                .executors(request.getExecutors() == null ? null
                                   : request.getExecutors().stream()
                                           .map(id -> User.builder().id(id).build())
                                           .collect(Collectors.toList()))
                .files(request.getFiles() == null ? null
                               : request.getFiles().stream()
                                       .map(id -> File.builder().id(id).build())
                                       .collect(Collectors.toList()))
                .build();
        setBaseEntityPropertiesFromRequestDtoIntoEntity(request, ticket);
        return ticket;
    }
}
