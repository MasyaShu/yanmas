package ru.itterminal.botdesk.tickets.model;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.commons.model.BaseEntity;

import javax.persistence.*;

@Entity
@Table(name = "ticket_types")
@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class TicketType extends BaseEntity {

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @Column(nullable = false, length = 128)
    private String name;

    @Column
    private String comment;

    @Column(name = "is_predefined", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE",
            updatable = false)
    private Boolean isPredefinedForNewTicket;

    @Override
    public void generateDisplayName() {
        setDisplayName(name);
    }
}
