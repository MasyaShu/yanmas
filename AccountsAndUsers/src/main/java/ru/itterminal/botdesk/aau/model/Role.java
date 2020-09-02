package ru.itterminal.botdesk.aau.model;

import java.util.Objects;
import java.util.UUID;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import ru.itterminal.botdesk.commons.model.BaseEntity;

@Entity
@Table(name="role")
@Setter
@Getter
@AllArgsConstructor
public class Role extends BaseEntity {

    @Column (nullable = false)
    private String name;

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
                Objects.equals(getId(), role.getId()) &&
                Objects.equals(getVersion(), role.getVersion()) &&
                Objects.equals(getDeleted(), role.getDeleted());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getId(), name);
    }
}
