package ru.itterminal.botdesk.botdeskapp;

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
import ru.itterminal.botdesk.commons.repository.ParentEntityRepositoryImpl;

@SpringBootApplication(scanBasePackages = "ru.itterminal.botdesk")
@EnableTransactionManagement
@EnableJpaRepositories(basePackages = "ru.itterminal.botdesk", repositoryBaseClass = ParentEntityRepositoryImpl.class)
@RequiredArgsConstructor
public class BotDeskApp {

    private final DataSource dataSource;

    public static void main(String[] args) {
        SpringApplication.run(BotDeskApp.class, args);
    }

    @Bean
    public EntityManagerFactory entityManagerFactory() {
        HibernateJpaVendorAdapter vendorAdapter = new HibernateJpaVendorAdapter();
        vendorAdapter.setGenerateDdl(Boolean.FALSE);
        vendorAdapter.setShowSql(Boolean.TRUE);
        LocalContainerEntityManagerFactoryBean factory = new LocalContainerEntityManagerFactoryBean();
        factory.setJpaVendorAdapter(vendorAdapter);

        factory.setPackagesToScan("ru.itterminal.botdesk");
        factory.setDataSource(dataSource);
        factory.afterPropertiesSet();
        factory.setLoadTimeWeaver(new InstrumentationLoadTimeWeaver());
        return factory.getObject();
    }
}
