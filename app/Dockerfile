FROM haskell:8.0
COPY christmalt ./
WORKDIR christmalt
RUN apt-get update && apt-get install -y postgresql libpq-dev
RUN stack build
CMD stack exec christmalt

