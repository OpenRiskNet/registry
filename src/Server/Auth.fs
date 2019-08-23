module Orn.Registry.Auth

open Microsoft.AspNetCore.Authentication.JwtBearer
open Microsoft.IdentityModel.Tokens
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
let scheme = JwtBearerDefaults.AuthenticationScheme
let configureKeycloak (config : IConfiguration) (services : IServiceCollection) =
    printfn "Keycloak domain: %s" config.["Jwt:KeycloakDomain"]
    services
        .AddAuthentication(fun config ->
            config.DefaultAuthenticateScheme <- scheme
            config.DefaultChallengeScheme <- scheme)
        .AddJwtBearer(fun options ->
            printfn "Configuring jwt bearer options"
            options.Authority <- sprintf "https://%s" config.["Jwt:KeycloakDomain"]
            options.TokenValidationParameters <-
            TokenValidationParameters(
                ValidateAudience = false
      )
        )
    |> ignore
