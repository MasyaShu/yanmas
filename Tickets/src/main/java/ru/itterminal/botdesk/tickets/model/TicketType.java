package ru.itterminal.botdesk.tickets.model;

import java.util.Objects;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.aau.model.Account;
import ru.itterminal.botdesk.commons.model.BaseEntity;

@Entity
@Table(name = "ticket_types")
@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class TicketType extends BaseEntity {

    @ManyToOne
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @Column(nullable = false, length = 128)
    private String name;

    @Column
    private String comment;

    @Column(name = "is_predefined", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isPredefined;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        TicketType ticketType = (TicketType) o;
        return Objects.equals(name, ticketType.name) &&
                Objects.equals(comment, ticketType.comment) &&
                Objects.equals(isPredefined, ticketType.isPredefined) &&
                Objects.equals(account, ticketType.account) &&
                Objects.equals(getId(), ticketType.getId()) &&
                Objects.equals(getOutId(), ticketType.getOutId()) &&
                Objects.equals(getVersion(), ticketType.getVersion()) &&
                Objects.equals(getDeleted(), ticketType.getDeleted());
    }

    @Override
    public int hashCode() {
        return Objects.hash(account, name, comment, isPredefined,
                            getId(), getOutId(), getVersion(), getDeleted()
        );
    }

    @Override
    public void generateDisplayName() {

    }
}
