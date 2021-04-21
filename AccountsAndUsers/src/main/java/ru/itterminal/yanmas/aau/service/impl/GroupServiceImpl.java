package ru.itterminal.yanmas.aau.service.impl;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.itterminal.yanmas.aau.model.Group;
import ru.itterminal.yanmas.aau.repository.GroupRepository;
import ru.itterminal.yanmas.aau.service.validator.GroupOperationValidator;

@Slf4j
@Service
@Transactional
@RequiredArgsConstructor
public class GroupServiceImpl extends CrudServiceWithAccountImpl<Group, GroupOperationValidator, GroupRepository> {

}
