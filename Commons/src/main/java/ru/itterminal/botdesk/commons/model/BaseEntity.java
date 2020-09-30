package ru.itterminal.botdesk.commons.model;

import java.util.UUID;

import javax.persistence.Column;
import javax.persistence.Id;
import javax.persistence.MappedSuperclass;
import javax.persistence.Version;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@MappedSuperclass
@NoArgsConstructor
public class BaseEntity {
    @Id
    private UUID id;

    @Column(name = "out_id", nullable = false, length = 128)
    private String outId;

    @Column(name = "deleted", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean deleted;

    @Version
    private Integer version;

    /**
     * @deprecated for manual changes {@link BaseEntity#version}!
     * Only for Dto->Entity mapping.
     *
     * @param version
     */
    @Deprecated
    public void setVersion(Integer version) {
        this.version = version;
    }
}
