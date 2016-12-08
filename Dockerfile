FROM hseeberger/scala-sbt
ADD build.sbt build.sbt
ADD ext ext
ADD src src
RUN sbt package
CMD sbt run
