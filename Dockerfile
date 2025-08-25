FROM erlang:24.3.3

# Set working directory
WORKDIR /app

# Copy rebar config and lock files
COPY rebar.config rebar.lock* ./

# Get dependencies
RUN rebar3 get-deps

# Copy source code
COPY src/ ./src/

# Compile the application and build release
RUN rebar3 release

# Expose port 8080
EXPOSE 8080

# Start the application directly using the release
CMD ["./_build/default/rel/hello_app/bin/hello_app", "foreground"]