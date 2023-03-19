FROM debian:bullseye

RUN apt update && apt upgrade -y && apt install -y curl unzip

ARG JAVA_VERSION=17.0.4
ARG SBT_VERSION=1.7.1
ARG SCALA_VERSION=3.1.3

WORKDIR /usr/local

ARG JAVA_URL="https://download.oracle.com/java/17/archive/jdk-${JAVA_VERSION}_linux-x64_bin.tar.gz"
RUN curl -SsfLk ${JAVA_URL} -o jdk_linux-x64_bin.tar.gz
RUN mkdir tmp
RUN tar xzf jdk_linux-x64_bin.tar.gz -C tmp
RUN mv tmp/$(ls tmp) jdk
RUN rmdir tmp

ARG SCALA_URL="https://github.com/lampepfl/dotty/releases/download/${SCALA_VERSION}/scala3-${SCALA_VERSION}.tar.gz"
RUN curl -SsfLk ${SCALA_URL} -o scala.tgz
RUN mkdir tmp
RUN tar xzf scala.tgz -C tmp
RUN mv tmp/$(ls tmp) scala
RUN rmdir tmp

ARG SBT_URL="https://github.com/sbt/sbt/releases/download/v${SBT_VERSION}/sbt-${SBT_VERSION}.zip"
RUN curl -SsfLk ${SBT_URL} -o sbt.zip
RUN mkdir tmp
RUN unzip sbt.zip -d tmp
RUN mv tmp/$(ls tmp) sbt
RUN rmdir tmp

ENV JAVA_HOME /usr/local/jdk
ENV SCALA_HOME /usr/local/scala
ENV PATH /usr/local/sbt/bin:$SCALA_HOME/bin:$JAVA_HOME/bin:$PATH

RUN mkdir /tmp/.sbt/
RUN chmod -R 777 /tmp/.sbt/

COPY entrypoint.sh /usr/local/entrypoint.sh

