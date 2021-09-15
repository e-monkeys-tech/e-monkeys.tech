FROM alpine:latest

WORKDIR /opt

# need GNU tools and not busybox eponym binaries
RUN apk update && \
    apk --no-cache add util-linux pciutils usbutils coreutils binutils findutils \
    bash git grep wget && \
    apk --no-cache add zola --repository http://dl-cdn.alpinelinux.org/alpine/edge/community/ && \
    rm /bin/sh && \
    ln -s /bin/bash /bin/sh && \
    mkdir -p /opt/blog/{static,templates,themes,sass} 

COPY . /opt/blog

WORKDIR /opt/blog

RUN git init && \
    git submodule add https://github.com/janbaudisch/zola-sam.git themes/sam && \
    git submodule update --init --recursive

ENTRYPOINT ["zola", "serve"]