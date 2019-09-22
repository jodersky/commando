FROM debian:bullseye

RUN apt-get update && apt-get install --yes \
        curl \
        openjdk-11-jdk \
    && rm -rf /var/lib/apt/lists/*

RUN curl -L https://github.com/lihaoyi/mill/releases/download/0.5.1/0.5.1 > \
    /usr/local/bin/mill \
    && chmod +x /usr/local/bin/mill

ENTRYPOINT [ "/usr/local/bin/mill" ]
