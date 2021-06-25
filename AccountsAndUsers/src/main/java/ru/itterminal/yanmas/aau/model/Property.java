package ru.itterminal.yanmas.aau.model;

import lombok.*;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.commons.model.BaseEntity;

import javax.persistence.*;

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

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "property_group_id", nullable = false)
    private PropertyGroup propertyGroup;

    @Column(nullable = false, length = 256)
    private String name;

    @Column(name = "type_property", nullable = false, length = 256)
    private String typeProperty;

    @Column(name = "entity_name", nullable = false, length = 256)
    private String entityName;

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
