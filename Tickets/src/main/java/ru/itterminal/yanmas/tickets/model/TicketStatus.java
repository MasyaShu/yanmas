package ru.itterminal.yanmas.tickets.model;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.aau.model.Account;
import ru.itterminal.yanmas.commons.model.BaseEntity;

import javax.persistence.*;

@Entity
@Table(name = "ticket_statuses")
@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class TicketStatus extends BaseEntity {

    @ManyToOne
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @Column(nullable = false, length = 128)
    private String name;

    @Column(name = "sort_index", nullable = false)
    private Integer sortIndex;

    @Column(name = "is_started_predefined", nullable = false,
            columnDefinition = "BOOLEAN DEFAULT FALSE", updatable = false)
    private Boolean isStartedPredefined;

    @Column(name = "is_finished_predefined", nullable = false,
            columnDefinition = "BOOLEAN DEFAULT FALSE", updatable = false)
    private Boolean isFinishedPredefined;

    @Column(name = "is_reopened_predefined", nullable = false,
            columnDefinition = "BOOLEAN DEFAULT FALSE", updatable = false)
    private Boolean isReopenedPredefined;

    @Column(name = "is_canceled_predefined", nullable = false,
            columnDefinition = "BOOLEAN DEFAULT FALSE", updatable = false)
    private Boolean isCanceledPredefined;

    @Override
    public void generateDisplayName() {
        setDisplayName(name);
    }

    @PrePersist
    protected void onCreate() {
        setDeleted(false);
        if(getIsCanceledPredefined() == null) {
            setIsCanceledPredefined(false);
        }
        if(getIsFinishedPredefined() == null) {
            setIsFinishedPredefined(false);
        }
        if(getIsReopenedPredefined() == null) {
            setIsReopenedPredefined(false);
        }
        if(getIsStartedPredefined() == null) {
            setIsStartedPredefined(false);
        }
    }
}
