package ru.itterminal.yanmas.aau.service.expansion_spec;

import org.springframework.data.jpa.domain.Specification;

import ru.itterminal.yanmas.aau.model.User;
import ru.itterminal.yanmas.commons.model.BaseEntity;

public interface ExpansionSpec <E extends BaseEntity>{

    Specification<E> expansionSpec(Specification<E> specification, User currentUser);
}
