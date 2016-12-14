FROM postgres:9.6
RUN cp /usr/share/zoneinfo/Australia/Sydney /etc/localtime
COPY init/ /docker-entrypoint-initdb.d/
