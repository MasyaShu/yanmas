package ru.itterminal.botdesk.tickets.model;

import java.util.UUID;

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

    public static final UUID PREDEFINED_TICKET_STATUS_FOR_NEW_ID =
            UUID.fromString("b78653b8-6704-4777-92dc-831d6e27a36a");
    public static final UUID PREDEFINED_TICKET_STATUS_FOR_REOPEN_ID =
            UUID.fromString("efc5aa64-91ce-4cec-8ac5-7b8ebd3233a8");
    public static final UUID PREDEFINED_TICKET_STATUS_FOR_CANCEL_ID =
            UUID.fromString("e256cbe7-dd33-4caf-a853-2bb535215ddc");
    public static final UUID PREDEFINED_TICKET_STATUS_FOR_CLOSE_ID =
            UUID.fromString("80ab5c74-6607-470b-93ed-42d0c2333aab");

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
}
