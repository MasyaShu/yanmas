package ru.itterminal.botdesk.aau.config;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.integration.annotation.Gateway;
import org.springframework.integration.annotation.IntegrationComponentScan;
import org.springframework.integration.annotation.MessagingGateway;

@Configuration
@ComponentScan(basePackages = "ru.itterminal.botdesk")
public class AccountsAndUsersConfig {

}
