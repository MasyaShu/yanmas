package ru.itterminal.botdesk.commons.model;

import java.util.Objects;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

@Entity
@Table(name = "account")
@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
public class Account extends BaseEntity {

    @Column(nullable = false, length = 128)
    private String name;

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof Account)) {
            return false;
        }
        Account account = (Account) o;
        return Objects.equals(name, account.name) &&
                Objects.equals(getOutId(), account.getOutId()) &&
                Objects.equals(getId(), account.getId()) &&
                Objects.equals(getVersion(), account.getVersion()) &&
                Objects.equals(getDeleted(), account.getDeleted());
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }
}
