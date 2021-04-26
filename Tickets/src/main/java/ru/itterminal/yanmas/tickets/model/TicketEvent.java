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

    @Column(name = "is_comment_for_executors")
    private Boolean isCommentForExecutors;

    @Column(name = "created_at", nullable = false, updatable = false)
    private Long createdAt;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "created_by_id")
    private User createdBy;

    @SuppressWarnings("JpaDataSourceORMInspection")
    @OneToMany(fetch = FetchType.EAGER)
    @JoinColumn(name = "entity_id")
    private List<File> files;

    @Override
    public void generateDisplayName() {
        setDisplayName("Event created by " + createdBy.getName());
    }
}
