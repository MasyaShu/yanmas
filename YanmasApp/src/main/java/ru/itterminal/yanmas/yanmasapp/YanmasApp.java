package ru.itterminal.yanmas.yanmasapp;

import javax.persistence.EntityManagerFactory;
import javax.sql.DataSource;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.instrument.classloading.InstrumentationLoadTimeWeaver;
import org.springframework.orm.jpa.LocalContainerEntityManagerFactoryBean;
import org.springframework.orm.jpa.vendor.HibernateJpaVendorAdapter;
import org.springframework.transaction.annotation.EnableTransactionManagement;

import lombok.RequiredArgsConstructor;
import ru.itterminal.yanmas.commons.repository.ParentEntityRepositoryImpl;

@SpringBootApplication(scanBasePackages = "ru.itterminal.yanmas")
@EnableTransactionManagement
@EnableJpaRepositories(basePackages = "ru.itterminal.yanmas", repositoryBaseClass = ParentEntityRepositoryImpl.class)
@RequiredArgsConstructor
public class YanmasApp {

    private final DataSource dataSource;

    public static void main(String[] args) {
        SpringApplication.run(YanmasApp.class, args);
    }

    @Bean
    public EntityManagerFactory entityManagerFactory() {
        var vendorAdapter = new HibernateJpaVendorAdapter();
        vendorAdapter.setGenerateDdl(Boolean.FALSE);
        vendorAdapter.setShowSql(Boolean.TRUE);
        var factory = new LocalContainerEntityManagerFactoryBean();
        factory.setJpaVendorAdapter(vendorAdapter);

        factory.setPackagesToScan("ru.itterminal.yanmas");
        factory.setDataSource(dataSource);
        factory.afterPropertiesSet();
        factory.setLoadTimeWeaver(new InstrumentationLoadTimeWeaver());
        return factory.getObject();
    }
}
