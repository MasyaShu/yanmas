package ru.itterminal.botdesk.commons.model;

import java.time.ZonedDateTime;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * Example of typical entity on microservice backoffice
 * Use only in unit tests
 */
@SuppressWarnings("ALL")
@Entity
@Table(name = "test_table")
@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class GeneralEntity extends BaseEntity {

    @Column(name = "name", unique = true)
    private String name;

    @Column(name = "date")
    private ZonedDateTime date;

    @Column(name = "description")
    private String description;

    @Override
    public String toString() {
        return "GeneralEntity{" +
            "id='" + this.getId() + '\'' +
            ", version='" + this.getVersion() + '\'' +
            ", deleted='" + this.getDeleted() + '\'' +
            ", name='" + name + '\'' +
            ", date=" + date +
            ", description='" + description + '\'' +
            '}';
    }
}
