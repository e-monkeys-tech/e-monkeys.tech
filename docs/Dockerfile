FROM rust:latest as builder
LABEL MAINTAINER="pbackz pierre.baconnier@outlook.fr"
LABEL REPOSITORY="https://github.com/pbackz/dockerfiles"

ENV APP="docs"
RUN USER=root cargo new --bin ${APP}
ENV USER=app_rust
ENV TZ="Europe/Paris"
ENV APP_WD="/opt/${APP}"

COPY . ${APP_WD}
WORKDIR ${APP_WD}

RUN rustup toolchain install nightly && \
    rustup override set nightly && \
    cargo install mdbook 

USER ${USER}

CMD ["mdbook", "serve"]