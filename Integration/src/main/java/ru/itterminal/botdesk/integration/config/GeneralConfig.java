package ru.itterminal.botdesk.integration.config;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.integration.annotation.IntegrationComponentScan;
import org.springframework.integration.config.EnableIntegration;

@Configuration
@EnableIntegration
@ComponentScan(basePackages = "ru.itterminal.botdesk")
@IntegrationComponentScan(basePackages = "ru.itterminal.botdesk.integration.aws")
public class GeneralConfig {

}
