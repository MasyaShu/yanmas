package ru.itterminal.botdesk.tickets.model;

import java.util.List;

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
import ru.itterminal.botdesk.commons.model.BaseEntity;

@Entity
@Table(name = "group_ticket_types")
@Getter
@Setter
@SuperBuilder(toBuilder = true)
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class GroupTicketTypes extends BaseEntity {

    @ManyToOne
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @Column(nullable = false, length = 256)
    private String name;

    @OneToMany(fetch = FetchType.LAZY)
    @JoinTable(
            name = "ticket_types_in_group",
            joinColumns = @JoinColumn(name = "group_of_ticket_types_id"),
            inverseJoinColumns = @JoinColumn(name = "ticket_type_id")
    )
    private List<TicketType> ticketTypes;

    @Override
    public void generateDisplayName() {
        setDisplayName(name);
    }

    @PrePersist
    protected void onCreate() {
        setDeleted(false);
    }


}
