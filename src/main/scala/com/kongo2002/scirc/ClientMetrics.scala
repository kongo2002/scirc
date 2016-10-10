package com.kongo2002.scirc

import kamon.Kamon
import kamon.metric.EntityRecorderFactory
import kamon.metric.GenericEntityRecorder
import kamon.metric.instrument.InstrumentFactory

class ClientMetrics(instrumentFactory: InstrumentFactory) extends GenericEntityRecorder(instrumentFactory) {
  val privateMessageCounter = Kamon.metrics.counter("private-messages")
}

object ClientMetrics extends EntityRecorderFactory[ClientMetrics] {
  override def category = "client-actor"
  override def createRecorder(instrumentFactory: InstrumentFactory): ClientMetrics =
    new ClientMetrics(instrumentFactory)
}
