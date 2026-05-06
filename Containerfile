# Stage 1: Build using erlang:26 as the base (matching glibc)
FROM erlang:26 AS builder
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain stable
ENV PATH="/root/.cargo/bin:${PATH}"
WORKDIR /workspace
COPY . .
RUN cargo build --release

# Stage 2: Runtime (same base, no glibc mismatch)
FROM erlang:26
WORKDIR /app
COPY --from=builder /workspace/target/release/gst .
CMD ["./gst"]
