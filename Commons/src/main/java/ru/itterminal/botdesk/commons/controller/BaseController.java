package ru.itterminal.botdesk.commons.controller;

import static java.util.stream.Collectors.toList;

import java.util.ArrayList;
import java.util.List;

import org.modelmapper.ModelMapper;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;

public abstract class BaseController {

    public static final String CREATE_INIT_MESSAGE = "Get request for create new {}: {}";
    public static final String UPDATE_INIT_MESSAGE = "Get request for update {}: {}";
    public static final String FIND_BY_ID_INIT_MESSAGE = "Get id for find {}: {}";
    public static final String FIND_ALL_INIT_MESSAGE = "Get all entities for {}";
    public static final String FIND_INIT_MESSAGE = "Get request for find {}, page: {}, size: {}, filter: {}";
    public static final String MARK_DELETE_INIT_MESSAGE =
            "Get request for mark delete {}, id: {}, version: {}, deleted: {}";

    public static final String CREATE_FINISH_MESSAGE = "Done request for create new {}: {}";
    public static final String UPDATE_FINISH_MESSAGE = "Done request for update {}: {}";
    public static final String FIND_BY_ID_FINISH_MESSAGE = "Done find by id {}: {}";
    public static final String FIND_ALL_FINISH_MESSAGE = "Done find all entities for {}";
    public static final String FIND_FINISH_MESSAGE = "Done request for find {}, found count: {}";
    public static final String MARK_DELETE_FINISH_MESSAGE =
            "Done request for mark delete {}, id: {}, version: {}, deleted: {}";

    public static final String UUID_NOT_NULL = "UUID mustn't be null";
    public static final String INVALID_PAGE_NUMBER_MSG = "invalid page number or size";
    public static final String INVALID_PAGE_NUMBER_CODE = "1016";

    public static final String SIZE_DEFAULT_VALUE = "25";
    public static final String PAGE_DEFAULT_VALUE = "0";

    public static final String CAN_NOT_BECAUSE_LOGICAL_DELETED = "Can't %s new %s because it is logical deleted: %s";

    protected ModelMapper modelMapper = new ModelMapper();

    public <S, T> Page<T> mapPage(Page<S> source, Class<T> targetClass, Pageable pageable) {
        return new PageImpl<>(source.getContent()
                .stream()
                .map(element -> modelMapper.map(element, targetClass))
                .collect(toList()),
                pageable, source.getTotalElements());
    }

    public <S, T> List<T> mapList(List<S> source, Class<T> targetClass) {
        List<T> result = new ArrayList<>();
        source.forEach(entity -> {
            result.add(modelMapper.map(entity, targetClass));
        });
        return result;
    }
}
