FROM douglasconnect/safe-stack-build-container:latest as builder

RUN mkdir -p /root/build
WORKDIR /root/build

RUN dotnet tool install -g paket
RUN dotnet tool install -g fake-cli
ENV PATH=$PATH:/root/.dotnet/tools

COPY ./paket.dependencies ./
COPY ./paket.lock ./

RUN paket restore

COPY ./src/ ./src/
COPY ./package.json ./package.json
COPY ./yarn.lock ./yarn.lock

COPY ./build.fsx .

WORKDIR /root/build
RUN fake build -f build.fsx --target Bundle

FROM  mcr.microsoft.com/dotnet/core/aspnet:3.0
WORKDIR /registry
COPY --from=builder /root/build/deploy .
EXPOSE 8085/tcp
WORKDIR /registry/Server
RUN chgrp -R 0 /registry/Server && \
    chmod -R g=u /registry/Server
CMD ["dotnet", "Server.dll"]
ARG GIT_CHANGESET
RUN echo "$GIT_CHANGESET" > /git_changeset_info
USER 1023
