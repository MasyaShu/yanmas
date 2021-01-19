package ru.itterminal.botdesk.commons.model.spec;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import ru.itterminal.botdesk.commons.model.BaseEntity;
import ru.itterminal.botdesk.commons.model.filter.BaseEntityFilter;
import ru.itterminal.botdesk.commons.model.filter.BooleanFilter;
import ru.itterminal.botdesk.commons.model.filter.ListOfBaseEntityFilter;
import ru.itterminal.botdesk.commons.model.filter.NumberFilter;
import ru.itterminal.botdesk.commons.model.filter.StringFilter;

@Component
public class SpecificationsFactory {

    private final StringFilterSpecificationsFactory stringFactory = new StringFilterSpecificationsFactory();
    private final NumberFilterSpecificationsFactory numberFactory = new NumberFilterSpecificationsFactory();
    private final BooleanFilterSpecificationsFactory booleanFactory = new BooleanFilterSpecificationsFactory();
    private final ListOfBaseEntityFilterSpecificationsFactory listBaseEntityFactory =
            new ListOfBaseEntityFilterSpecificationsFactory();
    private final BaseEntityFilterSpecificationsFactory baseEntityFactory = new BaseEntityFilterSpecificationsFactory();

    public <E extends BaseEntity> Specification<E> makeSpecification
            (Class<E> entityClass, String field, Object filter) {
        String clazzFilterName = filter.getClass().getSimpleName();
        return switch (clazzFilterName) {
            case "StringFilter" -> makeSpecificationForStringFilter(filter, field);
            case "BooleanFilter" -> makeSpecificationForBooleanFilter(filter, field);
            case "NumberFilter" -> makeSpecificationForNumberFilter(filter, field);
            case "BaseEntityFilter" -> makeSpecificationForBaseEntityFilter(filter, field);
            case "ListOfBaseEntityFilter" -> makeSpecificationForListOfBaseEntityFilter(entityClass, field, filter);
            default -> throw new IllegalStateException("Unexpected value: " + clazzFilterName);
        };
    }

    private <E extends BaseEntity> Specification<E> makeSpecificationForStringFilter(Object filter, String field) {
        var stringFilter = (StringFilter) filter;
        return stringFactory.makeSpecification(stringFilter, field);
    }

    private <E extends BaseEntity> Specification<E> makeSpecificationForBooleanFilter(Object filter, String field) {
        var booleanFilter = (BooleanFilter) filter;
        return booleanFactory.makeSpecification(booleanFilter, field);
    }

    private <E extends BaseEntity> Specification<E> makeSpecificationForNumberFilter(Object filter, String field) {
        var numberFilter = (NumberFilter) filter;
        return numberFactory.makeSpecification(numberFilter, field);
    }

    private <E extends BaseEntity> Specification<E> makeSpecificationForBaseEntityFilter(Object filter, String field) {
        var baseEntityFilter = (BaseEntityFilter) filter;
        return baseEntityFactory.makeSpecification(baseEntityFilter, field);
    }

    private <E extends BaseEntity> Specification<E> makeSpecificationForListOfBaseEntityFilter
            (Class<E> entityClass, String field, Object filter) {
        var listOfBaseEntityFilter = (ListOfBaseEntityFilter) filter;
        return listBaseEntityFactory.makeSpecification(listOfBaseEntityFilter, field, entityClass);
    }
}
