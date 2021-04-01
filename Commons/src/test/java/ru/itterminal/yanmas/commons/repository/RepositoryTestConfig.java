package ru.itterminal.yanmas.commons.repository;

import org.springframework.boot.SpringBootConfiguration;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

/**
 * Unit-tests repository configs
 */
@SpringBootConfiguration
@EnableJpaRepositories(basePackages = "ru.itterminal.yanmas.commons.repository",
    repositoryBaseClass = ParentEntityRepositoryImpl.class)
@EntityScan(basePackages = "ru.itterminal.yanmas.commons.model")
public class RepositoryTestConfig {
}
