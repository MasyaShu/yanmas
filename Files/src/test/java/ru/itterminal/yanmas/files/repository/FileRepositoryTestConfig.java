package ru.itterminal.yanmas.files.repository;

import org.springframework.boot.SpringBootConfiguration;
import org.springframework.boot.autoconfigure.domain.EntityScan;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

import ru.itterminal.yanmas.commons.repository.ParentEntityRepositoryImpl;

@SpringBootConfiguration
@EnableJpaRepositories(basePackages = "ru.itterminal.yanmas",
    repositoryBaseClass = ParentEntityRepositoryImpl.class)
@EntityScan(basePackages = "ru.itterminal.yanmas")
public class FileRepositoryTestConfig {
}
