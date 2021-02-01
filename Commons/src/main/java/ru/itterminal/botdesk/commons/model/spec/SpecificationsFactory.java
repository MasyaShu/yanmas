package ru.itterminal.botdesk.commons.model.spec;

import static ru.itterminal.botdesk.commons.model.filter.BaseEntityFilter.TypeComparisonForBaseEntityFilter.EXIST_IN;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Component;

import ru.itterminal.botdesk.commons.model.BaseEntity;
import ru.itterminal.botdesk.commons.model.dto.BaseFilterDto;
import ru.itterminal.botdesk.commons.model.filter.BaseEntityFilter;
import ru.itterminal.botdesk.commons.model.filter.BooleanFilter;
import ru.itterminal.botdesk.commons.model.filter.ListOfBaseEntityFilter;
import ru.itterminal.botdesk.commons.model.filter.NumberFilter;
import ru.itterminal.botdesk.commons.model.filter.StringFilter;

@Component
public class SpecificationsFactory {

    public static final String ACCOUNT = "account";
    public static final String OUT_ID = "outId";
    public static final String DELETED = "deleted";
    public static final String STRING_FILTER = "StringFilter";
    public static final String BOOLEAN_FILTER = "BooleanFilter";
    public static final String NUMBER_FILTER = "NumberFilter";
    public static final String BASE_ENTITY_FILTER = "BaseEntityFilter";
    public static final String LIST_OF_BASE_ENTITY_FILTER = "ListOfBaseEntityFilter";

    private final StringFilterSpecificationsFactory stringFactory = new StringFilterSpecificationsFactory();
    private final NumberFilterSpecificationsFactory numberFactory = new NumberFilterSpecificationsFactory();
    private final BooleanFilterSpecificationsFactory booleanFactory = new BooleanFilterSpecificationsFactory();
    private final ListOfBaseEntityFilterSpecificationsFactory listBaseEntityFactory =
            new ListOfBaseEntityFilterSpecificationsFactory();
    private final BaseEntityFilterSpecificationsFactory baseEntityFactory = new BaseEntityFilterSpecificationsFactory();

    public <E extends BaseEntity, F extends BaseFilterDto> Specification<E> makeSpecificationFromEntityFilterDto
            (Class<E> entityClass, F filterDto, UUID accountId) {

        var filterForAccount = BaseEntityFilter.builder()
                .typeComparison(EXIST_IN.toString())
                .listOfIdEntities(List.of(accountId))
                .build();

        Specification<E> accountSpec = baseEntityFactory.makeSpecification(filterForAccount, ACCOUNT);
        Specification<E> returnedSpec = Specification.where(accountSpec);

        if (filterDto.getOutId() != null) {
            Specification<E> outIdSpec = stringFactory.makeSpecification(filterDto.getOutId(), OUT_ID);
            returnedSpec = returnedSpec.and(outIdSpec);
        }

        if (filterDto.getDeleted() != null) {
            Specification<E> deletedSpec = booleanFactory.makeSpecification(filterDto.getDeleted(), DELETED);
            returnedSpec = returnedSpec.and(deletedSpec);
        }

        Field[] fields = filterDto.getClass().getDeclaredFields();
        List<Specification<E>> createdSpec = new ArrayList<>();
        for (Field field : fields) {
            try {
                field.setAccessible(true);
                var filter = field.get(filterDto);
                if (filter != null) {
                    var fieldName = field.getName();
                    var spec = makeSpecification(entityClass, fieldName, filter);
                    if (spec != null) {
                        createdSpec.add(spec);
                    }
                }
            }
            catch (IllegalAccessException e) {
                e.printStackTrace();
            }
        }
        for (Specification<E> spec : createdSpec) {
            returnedSpec = returnedSpec.and(spec);
        }
        return returnedSpec;
    }

    public <E extends BaseEntity> Specification<E> makeSpecification
            (Class<E> entityClass, String field, Object filter) {
        String clazzFilterName = filter.getClass().getSimpleName();
        return switch (clazzFilterName) {
            case STRING_FILTER -> makeSpecificationForStringFilter(filter, field);
            case BOOLEAN_FILTER -> makeSpecificationForBooleanFilter(filter, field);
            case NUMBER_FILTER -> makeSpecificationForNumberFilter(filter, field);
            case BASE_ENTITY_FILTER -> makeSpecificationForBaseEntityFilter(filter, field);
            case LIST_OF_BASE_ENTITY_FILTER -> makeSpecificationForListOfBaseEntityFilter(entityClass, field, filter);
            default -> null;
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
