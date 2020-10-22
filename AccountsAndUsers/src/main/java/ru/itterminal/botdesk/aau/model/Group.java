package ru.itterminal.botdesk.aau.model;

import lombok.*;
import ru.itterminal.botdesk.commons.model.BaseEntity;

import javax.persistence.*;
import java.util.Objects;

@Entity
@Table(name = "group_users")
@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class Group extends BaseEntity {

    @Column(nullable = false, length = 128)
    private String name;

    @Column
    private String comment;

    @Column(name = "is_inner", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isInner;

    @Column(name = "is_deprecated", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isDeprecated;

    @ManyToOne
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Group group = (Group) o;
        return Objects.equals(name, group.name) &&
                Objects.equals(comment, group.comment) &&
                Objects.equals(isInner, group.isInner) &&
                Objects.equals(isDeprecated, group.isInner) &&
                Objects.equals(account, group.account) &&
                Objects.equals(getId(), group.getId()) &&
                Objects.equals(getOutId(), group.getOutId()) &&
                Objects.equals(getVersion(), group.getVersion()) &&
                Objects.equals(getDeleted(), group.getDeleted());
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, comment, isInner, isDeprecated, account,
                getId(), getOutId(), getVersion(), getDeleted());
    }
}
