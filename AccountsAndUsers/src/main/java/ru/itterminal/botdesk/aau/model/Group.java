package ru.itterminal.botdesk.aau.model;

import java.util.Objects;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.BaseEntityWithAccount;

@Entity
@Table(name = "group_users")
@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class Group extends BaseEntityWithAccount {

    @Column(nullable = false, length = 128)
    private String name;

    @Column
    private String comment;

    @Column(name = "is_inner", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isInner;

    @Column(name = "is_deprecated", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isDeprecated;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Group group = (Group) o;
        return Objects.equals(name, group.name) &&
                Objects.equals(comment, group.comment) &&
                Objects.equals(isInner, group.isInner) &&
                Objects.equals(isDeprecated, group.isDeprecated) &&
                Objects.equals(getAccount(), group.getAccount()) &&
                Objects.equals(getId(), group.getId()) &&
                Objects.equals(getOutId(), group.getOutId()) &&
                Objects.equals(getVersion(), group.getVersion()) &&
                Objects.equals(getDeleted(), group.getDeleted());
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, comment, isInner, isDeprecated,
                getId(), getOutId(), getVersion(), getDeleted(), getAccount());
    }
}
