package ru.itterminal.botdesk.aau.repository;

import org.springframework.boot.SpringBootConfiguration;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import ru.itterminal.botdesk.commons.repository.ParentEntityRepositoryImpl;

/**
 * Unit-tests repository configs
 */
@SpringBootConfiguration
@EnableJpaRepositories(basePackages = "ru.itterminal.botdesk.aau.repository",
    repositoryBaseClass = ParentEntityRepositoryImpl.class)
@EntityScan(basePackages = "ru.itterminal.botdesk.aau.model")
public class UserRepositoryTestConfig {
}
