package ru.itterminal.yanmas.tickets.model;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.aau.model.Account;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.commons.model.BaseEntity;
import ru.itterminal.yanmas.files.model.File;

import javax.persistence.*;
import java.util.List;
import java.util.UUID;

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

    @Column(name = "is_comment_for_executors")
    private Boolean isCommentForExecutors;

    @Column(name = "created_at", nullable = false, updatable = false)
    private Long createdAt;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "created_by_id")
    private User createdBy;

    @SuppressWarnings("JpaDataSourceORMInspection")
    @OneToMany(fetch = FetchType.LAZY)
    @JoinColumn(name = "entity_id")
    private List<File> files;

    @Override
    public void generateDisplayName() {
        var trimComment = "Event created by " + createdBy.getName();
        setDisplayName(trimComment);
    }
}
