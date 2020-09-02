package ru.itterminal.botdesk.commons.repository;

import org.springframework.boot.SpringBootConfiguration;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

/**
 * Unit-tests repository config
 */
@SpringBootConfiguration
@EnableJpaRepositories(basePackages = "ru.itterminal.botdesk.commons.repository",
    repositoryBaseClass = ParentEntityRepositoryImpl.class)
@EntityScan(basePackages = "ru.itterminal.botdesk.commons.model")
public class RepositoryTestConfig {
}
