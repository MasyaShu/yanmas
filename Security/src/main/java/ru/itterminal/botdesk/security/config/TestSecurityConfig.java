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
    public static final String AUTHOR_ACCOUNT_1_IS_INNER_GROUP_ID = "4a45a88d-8500-4157-8460-4b4f5a42b731";
    public static final String OWNER_ACCOUNT_1_IS_INNER_GROUP_ID = "55827ea4-f7bf-411e-897e-bc29da2cca65";
    public static final String OWNER_ACCOUNT_2_IS_INNER_GROUP_ID = "4c91bfe2-30bf-4ceb-b6a0-ca94fb04c139";
    public static final String ADMIN_ACCOUNT_1_IS_INNER_GROUP_ID = "d0d6f6b6-0443-4394-911c-c843e32fd454";
    public static final String ADMIN_ACCOUNT_2_IS_INNER_GROUP_ID = "be3bb7b9-3be8-4054-8f6a-3c9b8c1466d9";
    public static final String ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP_ID = "d17e76db-6abd-4efe-a423-097c89d430ca";
    public static final String ADMIN_ACCOUNT_2_IS_NOT_INNER_GROUP_ID = "754821aa-00c6-424b-a433-11a429d5de23";
    public static final String AUTHOR_ACCOUNT_1_IS_NOT_INNER_GROUP_ID = "a4a66812-c5d2-4ebb-8b50-2387fd70f049";
    public static final String EXECUTOR_ACCOUNT_1_IS_NOT_INNER_GROUP_ID = "e9178ed9-e910-4121-b206-b2530df2731e";
    public static final String EXECUTOR_ACCOUNT_1_IS_INNER_GROUP_ID = "f447e752-ed02-4acc-9cb1-3ae2a53f10f6";
    public static final String GROUP_1_ID = "0223e51a-4bb2-44ee-bc8e-1f047a2145e7";
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
                        .id(UUID.fromString(OWNER_ACCOUNT_1_IS_INNER_GROUP_ID))
                        .accountId(UUID.fromString(ACCOUNT_1_ID))
                        .weightRole(50)
                        .username(EMAIL_1)
                        .isInnerGroup(true)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_ACCOUNT_OWNER)))
                        .build();
                case "OWNER_ACCOUNT_2_IS_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .id(UUID.fromString(OWNER_ACCOUNT_2_IS_INNER_GROUP_ID))
                        .accountId(UUID.fromString(ACCOUNT_2_ID))
                        .weightRole(50)
                        .username(EMAIL_1)
                        .isInnerGroup(true)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_ACCOUNT_OWNER)))
                        .build();
                case "ADMIN_ACCOUNT_1_IS_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .id(UUID.fromString(ADMIN_ACCOUNT_1_IS_INNER_GROUP_ID))
                        .accountId(UUID.fromString(ACCOUNT_1_ID))
                        .weightRole(40)
                        .isInnerGroup(true)
                        .username(EMAIL_1)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_ADMIN)))
                        .build();
                case "ADMIN_ACCOUNT_2_IS_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .id(UUID.fromString(ADMIN_ACCOUNT_2_IS_INNER_GROUP_ID))
                        .accountId(UUID.fromString(ACCOUNT_2_ID))
                        .weightRole(40)
                        .isInnerGroup(true)
                        .username(EMAIL_1)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_ADMIN)))
                        .build();
                case "AUTHOR_ACCOUNT_1_IS_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .id(UUID.fromString(AUTHOR_ACCOUNT_1_IS_INNER_GROUP_ID))
                        .accountId(UUID.fromString(ACCOUNT_1_ID))
                        .weightRole(20)
                        .isInnerGroup(true)
                        .username(EMAIL_1)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_AUTHOR)))
                        .build();
                case "ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .id(UUID.fromString(ADMIN_ACCOUNT_1_IS_NOT_INNER_GROUP_ID))
                        .accountId(UUID.fromString(ACCOUNT_1_ID))
                        .groupId(UUID.fromString(GROUP_1_ID))
                        .weightRole(40)
                        .isInnerGroup(false)
                        .username(EMAIL_1)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_ADMIN)))
                        .build();
                case "ADMIN_ACCOUNT_2_IS_NOT_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .id(UUID.fromString(ADMIN_ACCOUNT_2_IS_NOT_INNER_GROUP_ID))
                        .accountId(UUID.fromString(ACCOUNT_2_ID))
                        .weightRole(40)
                        .groupId(UUID.fromString(GROUP_1_ID))
                        .isInnerGroup(false)
                        .username(EMAIL_1)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_ADMIN)))
                        .build();
                case "AUTHOR_ACCOUNT_1_IS_NOT_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .id(UUID.fromString(AUTHOR_ACCOUNT_1_IS_NOT_INNER_GROUP_ID))
                        .accountId(UUID.fromString(ACCOUNT_1_ID))
                        .groupId(UUID.fromString(GROUP_1_ID))
                        .weightRole(20)
                        .isInnerGroup(false)
                        .username(EMAIL_1)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_AUTHOR)))
                        .build();
                case "EXECUTOR_ACCOUNT_1_IS_NOT_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .id(UUID.fromString(EXECUTOR_ACCOUNT_1_IS_NOT_INNER_GROUP_ID))
                        .accountId(UUID.fromString(ACCOUNT_1_ID))
                        .groupId(UUID.fromString(GROUP_1_ID))
                        .weightRole(30)
                        .isInnerGroup(false)
                        .username(EMAIL_1)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_EXECUTOR)))
                        .build();
                case "EXECUTOR_ACCOUNT_1_IS_INNER_GROUP" -> jwtUser = JwtUser
                        .builder()
                        .id(UUID.fromString(EXECUTOR_ACCOUNT_1_IS_INNER_GROUP_ID))
                        .accountId(UUID.fromString(ACCOUNT_1_ID))
                        .groupId(UUID.fromString(GROUP_1_ID))
                        .weightRole(30)
                        .isInnerGroup(true)
                        .username(EMAIL_1)
                        .enabled(true)
                        .authorities(List.of(new SimpleGrantedAuthority(ROLE_EXECUTOR)))
                        .build();
                default -> jwtUser = null;
            }
            return jwtUser;
        };
    }
}
