package ru.itterminal.botdesk.tickets.model;


import lombok.*;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.commons.model.BaseEntity;

import javax.persistence.*;
import java.util.Objects;

@Entity
@Table(name = "ticket_types")
@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class TicketTypes extends BaseEntity {

    @Column(nullable = false, length = 128)
    private String name;

    @Column
    private String comment;

    @Column(name = "is_predefined", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isPredefined;

    @ManyToOne
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        TicketTypes ticketTypes = (TicketTypes) o;
        return Objects.equals(name, ticketTypes.name) &&
                Objects.equals(comment, ticketTypes.comment) &&
                Objects.equals(isPredefined, ticketTypes.isPredefined)&&
                Objects.equals(account, ticketTypes.account) &&
                Objects.equals(getId(), ticketTypes.getId()) &&
                Objects.equals(getOutId(), ticketTypes.getOutId()) &&
                Objects.equals(getVersion(), ticketTypes.getVersion()) &&
                Objects.equals(getDeleted(), ticketTypes.getDeleted());
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, comment, isPredefined, account,
                getId(), getOutId(), getVersion(), getDeleted());
    }
}
