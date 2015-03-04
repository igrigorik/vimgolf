FROM ruby:2.2.0

MAINTAINER kramos

RUN apt-get update && apt-get install -y vim

RUN gem install vimgolf

COPY dockervimgolf.sh /usr/local/bin/wrapvimgolf

ENTRYPOINT ["wrapvimgolf"]

#Override with the your chosen excercise
CMD ["5462e3f41198b80002512673"]
