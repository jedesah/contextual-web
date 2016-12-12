FROM hseeberger/scala-sbt
ADD build.sbt build.sbt
RUN sbt update
ADD src src
ADD ext ext
RUN sbt package
CMD sbt run
