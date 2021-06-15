package ru.itterminal.yanmas.commons.model;

import lombok.*;
import lombok.experimental.SuperBuilder;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.PrePersist;
import javax.persistence.Table;
import java.util.UUID;

@Entity
@Table(name = "properties")
@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class Property extends BaseEntity {

    @Column(name = "account_id", nullable = false)
    private UUID accountId;

    @Column(nullable = false, length = 256)
    private String name;

    @Column(name = "type_property", nullable = false, length = 256)
    private String typeProperty;

    @Column(name = "type_entity", nullable = false, length = 256)
    private String typeEntity;

    @Column (name = "order_view")
    private Integer orderView;

    @Column
    private Long description;

    @PrePersist
    protected void onCreate() {
        setDeleted(false);
    }

    @Override
    public void generateDisplayName() {
        setDisplayName(name);
    }
}
