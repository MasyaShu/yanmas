package ru.itterminal.botdesk.tickets.repository;

import org.springframework.boot.SpringBootConfiguration;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import ru.itterminal.botdesk.commons.repository.ParentEntityRepositoryImpl;

@SpringBootConfiguration
@EnableJpaRepositories(basePackages = "ru.itterminal.botdesk.tickets.repository",
    repositoryBaseClass = ParentEntityRepositoryImpl.class)
@EntityScan(basePackages = "ru.itterminal.botdesk")
public class TicketTypesRepositoryTestConfig {
}
