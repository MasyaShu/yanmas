package ru.itterminal.botdesk.tickets.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.commons.model.BaseEntity;

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

    @Column(name = "is_started_predefined", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isStartedPredefined;

    @Column(name = "is_finished_predefined", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isFinishedPredefined;

    @Column(name = "is_reopened_predefined", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isReopenedPredefined;

    @Column(name = "is_canceled_predefined", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isCanceledPredefined;

    @Override
    public void generateDisplayName() {
        setDisplayName(name);
    }
}
