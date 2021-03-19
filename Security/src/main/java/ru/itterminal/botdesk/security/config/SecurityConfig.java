package ru.itterminal.botdesk.security.config;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.http.HttpMethod;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

import lombok.RequiredArgsConstructor;
import ru.itterminal.botdesk.security.jwt.CustomAccessDeniedHandler;
import ru.itterminal.botdesk.security.jwt.CustomAuthenticationEntryPoint;
import ru.itterminal.botdesk.security.jwt.JwtFilter;
import ru.itterminal.botdesk.security.jwt.JwtProvider;

@EnableGlobalMethodSecurity(prePostEnabled = true)
@EnableWebSecurity
@ComponentScan(basePackages = "ru.itterminal.botdesk")
@RequiredArgsConstructor
public class SecurityConfig extends WebSecurityConfigurerAdapter {

    private final JwtProvider jwtProvider;
    private final CustomAuthenticationEntryPoint customAuthenticationEntryPoint;
    private final CustomAccessDeniedHandler accessDeniedHandler;


    static final String[] ROLE_ACCOUNT_OWNER_ADMIN_EXECUTOR_AUTHOR = {"ACCOUNT_OWNER", "ADMIN", "EXECUTOR", "AUTHOR"};
    static final String[] ROLE_ACCOUNT_OWNER_ADMIN_EXECUTOR = {"ACCOUNT_OWNER", "ADMIN", "EXECUTOR"};
    static final String[] ROLE_ACCOUNT_OWNER_ADMIN = {"ACCOUNT_OWNER", "ADMIN"};
    static final String ROLE_ACCOUNT_OWNER = "ACCOUNT_OWNER";

    static final String[] AUTH_WHITELIST_PERMIT_ALL = {
            "/swagger-ui.html",
    };

    static final String[] AUTH_WHITELIST_ANONYMOUS_FOR_ANY_HTTP_METHODS = {
            "/api/v1/auth/signin",
            "/api/v1/auth/email-verify",
            "/api/v1/auth/request-password-reset",
            "/api/v1/auth/password-reset",
            "/api/v1/auth/token-refresh"
    };

    static final String[] AUTH_WHITELIST_ANONYMOUS_FOR_POST_HTTP_METHOD = {
            "/api/v1/account"
    };

    static final String[] AUTH_WHITELIST_AUTHENTICATED_FOR_GET_HTTP_METHOD = {
            "/api/v1/account/**",
            "/api/v1/group/**",
            "/api/v1/role/**",
            "/api/v1/user/**",
            "/api/v1/ticket/**",
            "/api/v1/ticket-counter/**",
            "/api/v1/ticket-status/**",
            "/api/v1/ticket-type/**"

    };

    static final String[] AUTH_WHITELIST_ACCOUNT_OWNER_ADMIN_EXECUTOR_AUTHOR_FOR_GET_HTTP_METHOD = {
            "/api/v1/ticket-setting/**"

    };

    static final String[] AUTH_WHITELIST_ACCOUNT_OWNER_ADMIN_EXECUTOR_FOR_GET_HTTP_METHOD = {
            "/api/v1/ticket-template/**"

    };

    static final String[] AUTH_WHITELIST_ACCOUNT_OWNER_FOR_GET_HTTP_METHOD = {
            "/api/v1/auth/request-email-update",
            "/api/v1/auth/email-update"
    };

    static final String[] AUTH_WHITELIST_AUTHENTICATED_FOR_ANY_HTTP_METHOD = {
            "/api/v1/file/**"
    };

    static final String[] AUTH_WHITELIST_ACCOUNT_OWNER_FOR_PUT_HTTP_METHOD = {
            "/api/v1/account"
    };

    static final String[] AUTH_WHITELIST_ACCOUNT_OWNER_ADMIN_FOR_POST_HTTP_METHOD = {
            "/api/v1/group",
            "/api/v1/user",
            "/api/v1/ticket-setting",
            "/api/v1/ticket-status",
            "/api/v1/ticket-type"
    };

    static final String[] AUTH_WHITELIST_ACCOUNT_OWNER_ADMIN_EXECUTOR_FOR_PUT_HTTP_METHOD = {
            "/api/v1/group",
            "/api/v1/user",
            "/api/v1/ticket-template"
    };

    static final String[] AUTH_WHITELIST_ACCOUNT_OWNER_ADMIN_EXECUTOR_FOR_POST_HTTP_METHOD = {
            "/api/v1/ticket-template"
    };

    static final String[] AUTH_WHITELIST_ACCOUNT_OWNER_ADMIN_FOR_PUT_HTTP_METHOD = {
            "/api/v1/ticket-counter",
            "/api/v1/ticket-setting",
            "/api/v1/ticket-status",
            "/api/v1/ticket-type"
    };

    static final String[] AUTH_WHITELIST_ACCOUNT_OWNER_ADMIN_EXECUTOR_AUTHOR_FOR_POST_HTTP_METHOD = {
            "/api/v1/ticket"

    };

    @SuppressWarnings("EmptyMethod")
    @Bean
    @Override
    public AuthenticationManager authenticationManagerBean() throws Exception {
        return super.authenticationManagerBean();
    }

    @Override
    protected void configure(HttpSecurity http) throws Exception {
        http
                .httpBasic().disable()
                .cors().and()
                .csrf().disable()
                .exceptionHandling().authenticationEntryPoint(customAuthenticationEntryPoint).and()
                .exceptionHandling().accessDeniedHandler(accessDeniedHandler).and()
                .authorizeRequests()
                .antMatchers(AUTH_WHITELIST_PERMIT_ALL).permitAll()
                .antMatchers(AUTH_WHITELIST_ANONYMOUS_FOR_ANY_HTTP_METHODS).anonymous()
                .antMatchers(AUTH_WHITELIST_AUTHENTICATED_FOR_ANY_HTTP_METHOD).authenticated()
                .antMatchers(HttpMethod.GET, AUTH_WHITELIST_AUTHENTICATED_FOR_GET_HTTP_METHOD).authenticated()
                .antMatchers(HttpMethod.POST, AUTH_WHITELIST_ANONYMOUS_FOR_POST_HTTP_METHOD).anonymous()
                .antMatchers(HttpMethod.PUT, AUTH_WHITELIST_ACCOUNT_OWNER_FOR_PUT_HTTP_METHOD)
                .hasAuthority(ROLE_ACCOUNT_OWNER)
                .antMatchers(HttpMethod.GET, AUTH_WHITELIST_ACCOUNT_OWNER_FOR_GET_HTTP_METHOD)
                .hasAuthority(ROLE_ACCOUNT_OWNER)
                .antMatchers(HttpMethod.POST, AUTH_WHITELIST_ACCOUNT_OWNER_ADMIN_FOR_POST_HTTP_METHOD)
                .hasAnyAuthority(ROLE_ACCOUNT_OWNER_ADMIN)
                .antMatchers(HttpMethod.PUT, AUTH_WHITELIST_ACCOUNT_OWNER_ADMIN_EXECUTOR_FOR_PUT_HTTP_METHOD)
                .hasAnyAuthority(ROLE_ACCOUNT_OWNER_ADMIN_EXECUTOR)
                .antMatchers(HttpMethod.POST, AUTH_WHITELIST_ACCOUNT_OWNER_ADMIN_EXECUTOR_AUTHOR_FOR_POST_HTTP_METHOD)
                .hasAnyAuthority(ROLE_ACCOUNT_OWNER_ADMIN_EXECUTOR_AUTHOR)
                .antMatchers(HttpMethod.PUT, AUTH_WHITELIST_ACCOUNT_OWNER_ADMIN_FOR_PUT_HTTP_METHOD)
                .hasAnyAuthority(ROLE_ACCOUNT_OWNER_ADMIN)
                .antMatchers(HttpMethod.GET, AUTH_WHITELIST_ACCOUNT_OWNER_ADMIN_EXECUTOR_AUTHOR_FOR_GET_HTTP_METHOD)
                .hasAnyAuthority(ROLE_ACCOUNT_OWNER_ADMIN_EXECUTOR_AUTHOR)
                .antMatchers(HttpMethod.POST, AUTH_WHITELIST_ACCOUNT_OWNER_ADMIN_EXECUTOR_FOR_POST_HTTP_METHOD)
                .hasAnyAuthority(ROLE_ACCOUNT_OWNER_ADMIN_EXECUTOR)
                .antMatchers(HttpMethod.GET, AUTH_WHITELIST_ACCOUNT_OWNER_ADMIN_EXECUTOR_FOR_GET_HTTP_METHOD)
                .hasAnyAuthority(ROLE_ACCOUNT_OWNER_ADMIN_EXECUTOR)
                .antMatchers("/**").denyAll().and()
                .addFilterBefore(new JwtFilter(jwtProvider), UsernamePasswordAuthenticationFilter.class)
                .sessionManagement().sessionCreationPolicy(SessionCreationPolicy.STATELESS);
        http.headers().frameOptions().disable();
        http.headers().xssProtection();
    }

    @Bean
    CorsConfigurationSource corsConfigurationSource() {
        return SecurityConfig.getCorsConfiguration();
    }

    public static UrlBasedCorsConfigurationSource getCorsConfiguration() {
        CorsConfiguration configuration = new CorsConfiguration();
        configuration.setAllowedOrigins(Collections.singletonList("*"));
        configuration.setAllowedMethods(Arrays.asList("GET", "POST", "DELETE", "PUT"));
        configuration.setAllowedHeaders(Collections.singletonList("*"));
        configuration.setAllowCredentials(true);
        UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
        source.registerCorsConfiguration("/**", configuration);
        return source;
    }

    public static boolean isUrlPermittedForAnonymousUser(String url) {
        return List.of(AUTH_WHITELIST_ANONYMOUS_FOR_ANY_HTTP_METHODS).contains(url)
                || List.of(AUTH_WHITELIST_ANONYMOUS_FOR_POST_HTTP_METHOD).contains(url);
    }
}
