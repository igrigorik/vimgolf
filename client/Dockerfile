FROM debian:stable-slim

RUN set -eux; \
	apt-get update; \
	apt-get install -y --no-install-recommends \
		ruby \
		vim \
	; \
	rm -rf /var/lib/apt/lists/*

RUN gem install vimgolf

RUN useradd -m -c 'Vim Golfer' golfer

COPY run-vimgolf.sh /usr/local/bin/run-vimgolf

USER golfer

WORKDIR /home/golfer

ENTRYPOINT ["/usr/local/bin/run-vimgolf"]

CMD []
