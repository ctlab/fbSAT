FROM openjdk:8-jre-alpine
COPY ./fbsat-cli/build/libs/fbSAT.jar /app/
WORKDIR /app
ENTRYPOINT ["java", "-jar", "fbSAT.jar"]
