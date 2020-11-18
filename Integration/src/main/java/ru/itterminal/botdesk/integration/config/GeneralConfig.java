package ru.itterminal.botdesk.integration.config;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.integration.annotation.IntegrationComponentScan;

@Configuration
@IntegrationComponentScan(basePackages = "ru.itterminal.botdesk")
@ComponentScan(basePackages = "ru.itterminal.botdesk")
public class GeneralConfig {

}
