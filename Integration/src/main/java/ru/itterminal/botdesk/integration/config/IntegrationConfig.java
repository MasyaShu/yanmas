package ru.itterminal.botdesk.integration.config;

import org.springframework.context.annotation.Configuration;
import org.springframework.integration.annotation.IntegrationComponentScan;
import org.springframework.integration.config.EnableIntegration;

@Configuration
@EnableIntegration
@IntegrationComponentScan(basePackages = "ru.itterminal.botdesk")
public class IntegrationConfig {

}
