FROM microsoft/dotnet:2.1-runtime
COPY /deploy /
WORKDIR /Server
EXPOSE 8085
ENTRYPOINT [ "dotnet", "Server.dll" ]
