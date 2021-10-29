FROM gcc:latest as builder
WORKDIR /build
COPY . .
RUN make

FROM gcc:latest
WORKDIR /usr/games/
EXPOSE 6000
COPY --from=builder /build/bin/circle /usr/games/circle
COPY --from=builder /build/world.tar /usr/games/
RUN tar -xvf world.tar && rm -f world.tar
RUN useradd -rm -d /usr/games/ -s /bin/bash -g root -G sudo circle
RUN chown -R circle:root /usr/games
USER circle
CMD ["/usr/games/circle"]
