package ru.itterminal.botdesk.aau.model;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.BaseEntity;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import java.util.Objects;

@Entity
@Table(name = "role")
@Setter
@Getter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class Role extends BaseEntity {

    @Column(nullable = false, unique = true, length = 30)
    private String name;
    private int weight;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof Role)) {
            return false;
        }
        Role role = (Role) o;
        return Objects.equals(name, role.name) &&
                Objects.equals(weight, role.weight) &&
                Objects.equals(getId(), role.getId()) &&
                Objects.equals(getVersion(), role.getVersion()) &&
                Objects.equals(getDeleted(), role.getDeleted());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getId(), name, weight);
    }

    @Override
    public void generateDisplayName() {
        setDisplayName(name);
    }
}
