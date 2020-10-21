package ru.itterminal.botdesk.aau.model;

import java.util.Objects;
import java.util.UUID;

import javax.persistence.*;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import ru.itterminal.botdesk.commons.model.BaseEntity;

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
    private String isInner;

    @Column(name = "parent_id")
    private UUID parentId;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Group group = (Group) o;
        return Objects.equals(name, group.name) &&
                Objects.equals(comment, group.comment) &&
                Objects.equals(isInner, group.isInner) &&
                Objects.equals(parentId, group.parentId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, comment, isInner, parentId);
    }
}
