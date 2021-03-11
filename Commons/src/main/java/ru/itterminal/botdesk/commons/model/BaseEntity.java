package ru.itterminal.botdesk.commons.model;

import java.util.UUID;

import javax.persistence.Column;
import javax.persistence.Id;
import javax.persistence.MappedSuperclass;
import javax.persistence.Version;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.SuperBuilder;
import ru.itterminal.botdesk.commons.model.dto.BaseEntityDto;

@Getter
@Setter
@MappedSuperclass
@NoArgsConstructor
@SuperBuilder(toBuilder = true)
@ToString
@EqualsAndHashCode
public abstract class BaseEntity {
    @Id
    private UUID id;

    @Column(name = "out_id", nullable = false, length = 128)
    private String outId;

    @Column(name = "deleted", nullable = false, columnDefinition = "BOOLEAN DEFAULT FALSE")
    private Boolean deleted;

    @Version
    private Integer version;

    @Column(name = "display_name", nullable = false, length = 256)
    private String displayName;

    /**
     * @param version of entity in database
     * @deprecated for manual changes {@link BaseEntity#version}!
     * Only for Dto->Entity mapping.
     */
    @SuppressWarnings("DeprecatedIsStillUsed")
    @Deprecated
    public void setVersion(Integer version) {
        this.version = version;
    }

    public abstract void generateDisplayName();

    public static <E extends BaseEntity, R extends BaseEntityDto> void setBaseEntityPropertiesFromRequestDtoIntoEntity(
            R request,
            E entity) {
        if (request.getId() != null) {
            entity.setId(request.getId());
        }
        if (request.getOutId() != null) {
            entity.setOutId(request.getOutId());
        }
        if (request.getDeleted() != null) {
            entity.setDeleted(request.getDeleted());
        }
        if (request.getVersion() != null) {
            entity.setVersion(request.getVersion());
        }
    }
}
