FROM eu.gcr.io/douglasconnect-docker/fsharp-mono-dotnetcore:latest as builder

RUN mkdir -p /root/build
WORKDIR /root/build

RUN dotnet tool install -g paket
ENV PATH=$PATH:/root/.dotnet/tools

COPY ./paket.dependencies ./
COPY ./paket.lock ./

RUN paket restore

COPY ./src/ ./src/
COPY ./package.json ./package.json
COPY ./yarn.lock ./yarn.lock

COPY ./build.fsx .

WORKDIR /root/build
RUN ~/.dotnet/tools/fake build -f build.fsx --target Bundle

FROM  microsoft/dotnet:2.2-aspnetcore-runtime
WORKDIR /root/
COPY --from=builder /root/build/deploy .
EXPOSE 8085/tcp
WORKDIR /root/Server
CMD ["dotnet", "Server.dll"]
ARG GIT_CHANGESET
RUN echo "$GIT_CHANGESET" > /git_changeset_info
