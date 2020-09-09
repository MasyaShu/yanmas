package ru.itterminal.botdesk;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;

// TODO localization messages or texts of email
@SpringBootApplication
public class BotdeskApplication {

	public static void main(String[] args) {
		SpringApplication.run(BotdeskApplication.class, args);
	}

	@Bean
	public BCryptPasswordEncoder bCryptPasswordEncoder() {
		return new BCryptPasswordEncoder();
	}


}
