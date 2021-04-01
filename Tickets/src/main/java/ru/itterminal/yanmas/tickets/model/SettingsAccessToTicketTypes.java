package ru.itterminal.yanmas.tickets.model;

import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.PrePersist;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.aau.model.Account;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.commons.model.BaseEntity;

@SuppressWarnings("DuplicatedCode")
@Entity
@Table(name = "settings_access_to_ticket_via_ticket_types")
@Getter
@Setter
@SuperBuilder(toBuilder = true)
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class SettingsAccessToTicketTypes extends BaseEntity {

    @ManyToOne
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @ManyToOne
    @JoinColumn(name = "group_id", nullable = false)
    private Group group;

    @ManyToOne
    @JoinColumn(name = "user_id", nullable = false)
    private User user;

    @ManyToOne
    @JoinColumn(name = "group_of_ticket_types_id", nullable = false)
    private GroupTicketTypes groupTicketTypes;

    @Override
    public void generateDisplayName() {
        String resultGroup = getGroup() != null && getGroup().getDisplayName() != null
                ? getGroup().getDisplayName()
                : null;
        String resultAuthor = getUser() != null && getUser().getDisplayName() != null
                ? getUser().getDisplayName()
                : null;

        if (resultGroup != null && resultAuthor != null) {
            setDisplayName(resultGroup + " " + resultAuthor);
        }

        if (resultGroup != null && resultAuthor == null) {
            setDisplayName(resultGroup);
        }

        if (resultGroup == null && resultAuthor != null) {
            setDisplayName(resultAuthor);
        }
    }

    @PrePersist
    protected void onCreate() {
        setDeleted(false);
    }


}
