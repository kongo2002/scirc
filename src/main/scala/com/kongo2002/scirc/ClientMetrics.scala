package com.kongo2002.scirc

import kamon.Kamon
import kamon.metric.EntityRecorderFactory
import kamon.metric.GenericEntityRecorder
import kamon.metric.instrument.InstrumentFactory

trait HasClientMetrics {
  protected def metrics: ClientMetrics
}

class ClientMetrics(instrumentFactory: InstrumentFactory) extends GenericEntityRecorder(instrumentFactory) {
  val privateMessageCounter = Kamon.metrics.counter(ClientMetrics.PrivateMessages)
  val sentBytesCounter = Kamon.metrics.counter(ClientMetrics.SentBytes)
}

object ClientMetrics extends EntityRecorderFactory[ClientMetrics] {
  val PrivateMessages = "private-messages"
  val SentBytes = "sent-bytes"

  override def category = "client-actor"
  override def createRecorder(instrumentFactory: InstrumentFactory): ClientMetrics =
    new ClientMetrics(instrumentFactory)

  def removeEntity(name: String): Boolean =
    Kamon.metrics.removeEntity(name, category)
}
