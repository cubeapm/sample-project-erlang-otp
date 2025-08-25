# Hello World Erlang/OTP Application

This is a sample app to demonstrate how to instrument Erlang/OTP application with **OpenTelemetry**. It contains source code for a simple Cowboy web server that serves "Hello World" and demonstrates tracing for HTTP requests and error handling. This repository has a docker compose file to set up the application conveniently.

The code is organized into multiple branches. The main branch has the Erlang app without any instrumentation. Other branches then build upon the main branch to add specific instrumentations as below:

| Branch                                                                         | Instrumentation | Code changes for instrumentation                                                                |
| ------------------------------------------------------------------------------ | --------------- | ----------------------------------------------------------------------------------------------- |
| [main](https://github.com/cubeapm/sample-project-erlang-otp/tree/main)                    | None            | -                                                                                               |
| [otel](https://github.com/cubeapm/sample-project-erlang-otp/tree/otel)                    | OpenTelemetry   | [main...otel](https://github.com/cubeapm/erlang-docker/compare/main...otel)                    |

## Setup

Clone this repository and go to the project directory. Then run the following commands

```bash
docker compose up --build
```

The Erlang app will now be available at `http://localhost:8080`.

The app has various API endpoints to demonstrate basic HTTP functionality and error handling. Check out [hello_handler.erl](src/hello_handler.erl) for the list of API endpoints.

## API Endpoints

- **GET /** - Root endpoint returning "Hello from OTP24 Cowboy!"
- **GET /param/:id** - Parameterized endpoint that logs the ID parameter
- **GET /exception** - Exception handling endpoint for testing error scenarios and stacktraces

### Main Branch
- Basic Cowboy configuration
- Simple HTTP server setup

### OTEL Branch
The application is configured via `config/sys.config`. Key configurations include:
- OpenTelemetry configuration with OTLP exporter
- Service name and version

To modify the OpenTelemetry configuration, edit `config/sys.config`. The main settings you might want to change are:
- `otlp_endpoint` - Where to send telemetry data
- `service.name` and `service.version` - Identify your service
- Sampling and export configurations

## Contributing

Please feel free to raise PR for any enhancements - additional service integrations, library version updates, documentation updates, etc.