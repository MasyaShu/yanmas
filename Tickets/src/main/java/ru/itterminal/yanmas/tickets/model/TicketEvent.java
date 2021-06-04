package ru.itterminal.yanmas.tickets.model;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.aau.model.Account;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.files.model.File;

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

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "ticket_id")
    private Ticket ticket;

    @Column
    private String comment;

    @Column(name = "auto_comment")
    private String autoComment;

    @Column(name = "created_at", nullable = false, updatable = false)
    private Long createdAt;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "created_by_id")
    private User createdBy;

    @SuppressWarnings("JpaDataSourceORMInspection")
    @OneToMany(fetch = FetchType.LAZY)
    @JoinColumn(name = "entity_id")
    private List<File> files;

    @OneToMany(fetch = FetchType.LAZY)
    @JoinTable(
            name = "ticket_event_recipients",
            joinColumns = @JoinColumn(name = "ticket_event_id"),
            inverseJoinColumns = @JoinColumn(name = "recipient_id")
    )
    private List<User> recipients;

    @PrePersist
    protected void onCreate() {
        createdAt = System.currentTimeMillis();
        setDeleted(false);
    }

    @Override
    public void generateDisplayName() {
        setDisplayName(null);
    }
}
