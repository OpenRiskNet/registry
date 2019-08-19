# OpenRiskNet registry

This repository contains the source code for the [OpenRiskNet](https://openrisknet.org/) Registry component. The Registry is a web app that runs inside an OpenRiskNet Virtual Research Environment (=OpenShift/Kubernetes cluster) and auto-discovers REST style APIs that provide an [OpenAPI](https://www.openapis.org/) definition with [OpenRiskNet service annotations](https://github.com/OpenRiskNet/home/wiki/Annotating-API-to-make-it-queryable).

## Architecture

The Registry is implemented with the F# programming language using the SAFE project scaffold that enables use of F# on both the client (transpiled to Javascript) as well as on the backend on .NET Core in a linux docker container.

The frontend connects to the backend and regularly polls the list of available services. The backend is implemented using a couple of agents that send each other messages for processing events and the usual ASP.NET core/Kestrel based HTTP server to answer requests by clients.

The agents use the F# MailboxProcessor class for implementation and provide a simplified IAgent interface that describes a read-only state and a way to post typed messages to the agent.

The **Kubernetes agent** uses the Kubernetes API to listen for Kubernetes services in any namespace. When it finds one, it checks if one of the OpenRiskNet Kubernetes/OpenShift annotations are present with the URL of the OpenAPI document to index and if so the post a message to the OpenApiProcessingAgent.

The **OpenAPI processing agent** receives requests to index (or remove) annotated OpenAPI documents to the service index.
It then performs the following steps:
1. download the OpenAPI json or yaml
2. parse the OpenAPI document
3. dereferences OpenAPI references so every definition is inlined (e.g. shared response definitions)
4. rename the OpenAPI top level x-orn-@context to @context to turn the document into a valid Json-LD document
5. parses the result as a Json-LD document into an in-memory triple store

Because this chain can take a few seconds, the OpenAPI processing agent is instantiated a couple of times (currently 19 agents at a time) for a balanced amount of concurrency that are wrapped by a load balancing agent.

The OpenAPI services agent is only concerned with keeping a list of active services and their state.

The FeedbackAgent is used to collect messages in a thread safe way.

## Build instructions

The easiest way to build the registry is to run a docker build on the Dockerfile.

If you want to build and run the registry locally you need the following installed:
* [dotnet core sdk](https://www.microsoft.com/net/download/) version 2.1 or higher
* the [paket package manager](https://fsprojects.github.io/Paket)
* the [fake build tool](https://fake.build/)
* [node version](https://nodejs.org/) >= 8 (and ideally yarn but npm should also work)
Once you have the prerequisites, just run `fake build -t run` to run an interactive development server with hot reloading of the client and server or `fake build` to build and package everything.


## Notes

OpenRiskNet[OpenRiskNet](https://openrisknet.org/) is a 3 year project funded by the European Commission within Horizon2020 EINFRA-22-2016 Programme (Grant Agreement 731075; start date 1 December 2016).
