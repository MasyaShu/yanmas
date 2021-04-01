package ru.itterminal.yanmas.aau.model;

import javax.persistence.*;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.yanmas.commons.model.BaseEntity;

@Entity
@Table(name = "group_users")
@Getter
@Setter
@SuperBuilder(toBuilder = true)
@AllArgsConstructor
@NoArgsConstructor
@ToString(callSuper = true)
@EqualsAndHashCode(callSuper = true)
public class Group extends BaseEntity {

    @Column(nullable = false, length = 128)
    private String name;

    @Column
    private String comment;

    @Column(name = "is_inner", nullable = false,
            columnDefinition = "BOOLEAN DEFAULT FALSE", updatable = false)
    private Boolean isInner;

    @Column(name = "is_deprecated", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean isDeprecated;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "account_id", nullable = false)
    private Account account;

    @Override
    public void generateDisplayName() {
        setDisplayName(name);
    }

    @PrePersist
    protected void onCreate() {
        setIsDeprecated(false);
        setDeleted(false);
    }
}
