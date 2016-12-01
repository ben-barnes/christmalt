FROM openjdk:8-jre

RUN mkdir flyway
ADD flyway-commandline-4.0.3.tar.gz .
WORKDIR flyway-4.0.3
COPY flyway.conf flyway.conf
COPY sql/ sql/

ADD wait-for-it.sh wait-for-it.sh
RUN ["chmod", "+x", "wait-for-it.sh" ]

CMD [ "./wait-for-it.sh", "christmalt_db:5432", "-s", "-t", "10", "--", "./flyway migrate" ]

