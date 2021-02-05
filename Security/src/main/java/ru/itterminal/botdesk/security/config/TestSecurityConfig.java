package ru.itterminal.botdesk.security.config;

import java.util.List;
import java.util.UUID;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Profile;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.web.cors.CorsConfigurationSource;

import ru.itterminal.botdesk.security.jwt.JwtUser;

@EnableGlobalMethodSecurity(securedEnabled = true, prePostEnabled = true)
@EnableWebSecurity
@Profile("Test")
public class TestSecurityConfig extends WebSecurityConfigurerAdapter {

    public static final String ACCOUNT_1_ID = "cdfa6483-0769-4628-ba32-efd338a716de";
    public static final String ACCOUNT_2_ID = "bcf98101-2a22-42bf-94cc-c900b50a0b69";
    public static final String INNER_GROUP_ID = "0223e51a-4bb2-44ee-bc8e-1f047a2145e7";
    public static final String NOT_INNER_GROUP_ID = "63c1940d-e323-47af-8265-dbf8089727de";
    public static final String ROLE_ADMIN = "ADMIN";
    public static final String ROLE_AUTHOR = "AUTHOR";
    public static final String ROLE_EXECUTOR = "EXECUTOR";
    public static final String ROLE_ACCOUNT_OWNER = "ACCOUNT_OWNER";

    public static final String EMAIL_1 = "Mackenzie.lang@gmail.com";

    @Override
    protected void configure(HttpSecurity http) throws Exception {
        http
                .httpBasic().disable()
                .cors().and()
                .csrf().disable()
                .authorizeRequests()
                .antMatchers(SecurityConfig.AUTH_WHITELIST_PERMIT_ALL).permitAll()
                .antMatchers(SecurityConfig.AUTH_WHITELIST_ANONYMOUS).anonymous()
                .anyRequest().authenticated().and()
                .sessionManagement().sessionCreationPolicy(SessionCreationPolicy.STATELESS);
        http.headers().frameOptions().disable();
        http.headers().xssProtection();
    }

    @Bean
    CorsConfigurationSource corsConfigurationSource() {
        return SecurityConfig.getCorsConfiguration();
    }

    @Bean
    public UserDetailsService userDetailsTestService() {
        return username -> {
            JwtUser jwtUser;
            switch (username) {
                case "OWNER_ACCOUNT_1_IS_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .accountId(UUID.fromString(ACCOUNT_1_ID))
                        .weightRole(50)
                        .username(EMAIL_1)
                        .isInnerGroup(true)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_ACCOUNT_OWNER)))
                        .build();
                case "OWNER_ACCOUNT_2_IS_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .accountId(UUID.fromString(ACCOUNT_2_ID))
                        .weightRole(50)
                        .username(EMAIL_1)
                        .isInnerGroup(true)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_ACCOUNT_OWNER)))
                        .build();
                case "ADMIN_ACCOUNT_1_IS_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .accountId(UUID.fromString(ACCOUNT_1_ID))
                        .groupId(UUID.fromString(INNER_GROUP_ID))
                        .weightRole(40)
                        .isInnerGroup(true)
                        .username(EMAIL_1)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_ADMIN)))
                        .build();
                case "ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .accountId(UUID.fromString(ACCOUNT_1_ID))
                        .groupId(UUID.fromString(NOT_INNER_GROUP_ID))
                        .weightRole(40)
                        .isInnerGroup(false)
                        .username(EMAIL_1)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_ADMIN)))
                        .build();
                case "ADMIN_ACCOUNT_2_IS_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .accountId(UUID.fromString(ACCOUNT_2_ID))
                        .groupId(UUID.fromString(INNER_GROUP_ID))
                        .weightRole(40)
                        .isInnerGroup(true)
                        .username(EMAIL_1)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_ADMIN)))
                        .build();
                case "ADMIN_ACCOUNT_2_IS_NOT_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .accountId(UUID.fromString(ACCOUNT_2_ID))
                        .groupId(UUID.fromString(NOT_INNER_GROUP_ID))
                        .weightRole(40)
                        .isInnerGroup(false)
                        .username(EMAIL_1)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_ADMIN)))
                        .build();
                case "EXECUTOR_ACCOUNT_1_IS_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .accountId(UUID.fromString(ACCOUNT_1_ID))
                        .groupId(UUID.fromString(INNER_GROUP_ID))
                        .weightRole(30)
                        .isInnerGroup(true)
                        .username(EMAIL_1)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_EXECUTOR)))
                        .build();
                case "EXECUTOR_ACCOUNT_1_IS_NOT_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .accountId(UUID.fromString(ACCOUNT_1_ID))
                        .groupId(UUID.fromString(NOT_INNER_GROUP_ID))
                        .weightRole(30)
                        .isInnerGroup(false)
                        .username(EMAIL_1)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_EXECUTOR)))
                        .build();
                case "EXECUTOR_ACCOUNT_2_IS_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .accountId(UUID.fromString(ACCOUNT_2_ID))
                        .groupId(UUID.fromString(INNER_GROUP_ID))
                        .weightRole(30)
                        .isInnerGroup(true)
                        .username(EMAIL_1)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_EXECUTOR)))
                        .build();
                case "EXECUTOR_ACCOUNT_2_IS_NOT_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .accountId(UUID.fromString(ACCOUNT_2_ID))
                        .groupId(UUID.fromString(NOT_INNER_GROUP_ID))
                        .weightRole(30)
                        .isInnerGroup(false)
                        .username(EMAIL_1)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_EXECUTOR)))
                        .build();
                case "AUTHOR_ACCOUNT_1_IS_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .accountId(UUID.fromString(ACCOUNT_1_ID))
                        .groupId(UUID.fromString(INNER_GROUP_ID))
                        .weightRole(20)
                        .isInnerGroup(true)
                        .username(EMAIL_1)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_AUTHOR)))
                        .build();
                case "AUTHOR_ACCOUNT_1_IS_NOT_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .accountId(UUID.fromString(ACCOUNT_1_ID))
                        .groupId(UUID.fromString(NOT_INNER_GROUP_ID))
                        .weightRole(20)
                        .isInnerGroup(false)
                        .username(EMAIL_1)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_AUTHOR)))
                        .build();
                case "AUTHOR_ACCOUNT_2_IS_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .accountId(UUID.fromString(ACCOUNT_2_ID))
                        .groupId(UUID.fromString(INNER_GROUP_ID))
                        .weightRole(20)
                        .isInnerGroup(true)
                        .username(EMAIL_1)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_AUTHOR)))
                        .build();
                case "AUTHOR_ACCOUNT_2_IS_NOT_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .accountId(UUID.fromString(ACCOUNT_2_ID))
                        .groupId(UUID.fromString(NOT_INNER_GROUP_ID))
                        .weightRole(20)
                        .isInnerGroup(false)
                        .username(EMAIL_1)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_AUTHOR)))
                        .build();
                case "OBSERVER_ACCOUNT_1_IS_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .accountId(UUID.fromString(ACCOUNT_1_ID))
                        .groupId(UUID.fromString(INNER_GROUP_ID))
                        .weightRole(10)
                        .isInnerGroup(true)
                        .username(EMAIL_1)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_AUTHOR)))
                        .build();
                case "OBSERVER_ACCOUNT_1_IS_NOT_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .accountId(UUID.fromString(ACCOUNT_1_ID))
                        .groupId(UUID.fromString(NOT_INNER_GROUP_ID))
                        .weightRole(10)
                        .isInnerGroup(false)
                        .username(EMAIL_1)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_AUTHOR)))
                        .build();
                case "OBSERVER_ACCOUNT_2_IS_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .accountId(UUID.fromString(ACCOUNT_2_ID))
                        .groupId(UUID.fromString(INNER_GROUP_ID))
                        .weightRole(10)
                        .isInnerGroup(true)
                        .username(EMAIL_1)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_AUTHOR)))
                        .build();
                case "OBSERVER_ACCOUNT_2_IS_NOT_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .accountId(UUID.fromString(ACCOUNT_2_ID))
                        .groupId(UUID.fromString(NOT_INNER_GROUP_ID))
                        .weightRole(10)
                        .isInnerGroup(false)
                        .username(EMAIL_1)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_AUTHOR)))
                        .build();
                default -> jwtUser = null;
            }
            return jwtUser;
        };
    }
}
