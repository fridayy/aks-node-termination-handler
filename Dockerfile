FROM erlang:25-alpine  as build

RUN mkdir /opt/aksnth
WORKDIR /opt/aksnth

COPY . /opt/aksnth

RUN apk add --update git  \
    && rebar3 as prod release

# ensure that the alpine version is the same
# that the alpine version used in the build step
FROM alpine:3.17 as application

RUN apk add --no-cache openssl libstdc++ ncurses-libs && \
    adduser -h /opt/aksnth -u 1000 -s /bin/sh -D unprivileged

COPY --from=build --chown=unprivileged:unprivileged /opt/aksnth/_build/prod/rel/aksnth /opt/aksnth

RUN ln -s /opt/aksnth/bin/* /usr/local/bin/

USER 1000
WORKDIR /opt/aksnth

CMD ["aksnth", "foreground"]
