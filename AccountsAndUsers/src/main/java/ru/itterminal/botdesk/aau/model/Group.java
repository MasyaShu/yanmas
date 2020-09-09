package ru.itterminal.botdesk.aau.model;

import java.util.Objects;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import ru.itterminal.botdesk.commons.model.BaseEntity;

@Entity
@Table(name = "group")
@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class Group extends BaseEntity {
    @Column(nullable = false)
    private String name;

    @Column
    private String comment;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof Group)) {
            return false;
        }
        Group group = (Group) o;
        return Objects.equals(name, group.name) &&
                Objects.equals(comment, group.comment) &&
                Objects.equals(getId(), group.getId()) &&
                Objects.equals(getOutId(), group.getOutId()) &&
                Objects.equals(getVersion(), group.getVersion()) &&
                Objects.equals(getDeleted(), group.getDeleted());
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, comment);
    }
}
