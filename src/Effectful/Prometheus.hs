module Effectful.Prometheus
  ( Metrics (..)
  , runPrometheusMetrics
  , increaseCounter
  , setGauge
  , increaseLabelledCounter
  , setLabelledGauge
  , observe
  , observeLabelled
  ) where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Dispatch.Static
import Effectful.Reader.Static
import Prometheus qualified as P

data Metrics metrics :: Effect where
  IncreaseCounter :: (metrics -> P.Counter) -> Metrics metrics m ()
  SetGauge :: (metrics -> P.Gauge) -> Double -> Metrics metrics m ()
  IncreaseLabelledCounter :: P.Label label => (metrics -> P.Vector label P.Counter) -> label -> Metrics metrics m ()
  SetLabelledGauge :: P.Label label => (metrics -> P.Vector label P.Gauge) -> label -> Double -> Metrics metrics m ()
  Observe :: (metrics -> P.Histogram) -> Double -> Metrics metrics m ()
  ObserveLabelled :: P.Label label => (metrics -> P.Vector label P.Histogram) -> label -> Double -> Metrics metrics m ()

type instance DispatchOf (Metrics metrics) = Dynamic

runPrometheusMetrics :: IOE :> es => metricType -> Eff (Metrics metricType : es) a -> Eff es a
runPrometheusMetrics metricContainer =
  reinterpret (runReader metricContainer) $ \_ -> \case
    IncreaseCounter getter -> do
      counter <- asks getter
      unsafeEff_ $ P.incCounter counter
    SetGauge getter value -> do
      gauge <- asks getter
      unsafeEff_ $ P.setGauge gauge value
    IncreaseLabelledCounter getter label -> do
      counter <- asks getter
      unsafeEff_ $ P.withLabel counter label P.incCounter
    SetLabelledGauge getter label value -> do
      gauge <- asks getter
      unsafeEff_ $ P.withLabel gauge label (`P.setGauge` value)
    Observe getter value -> do
      histogram <- asks getter
      unsafeEff_ $ P.observe histogram value
    ObserveLabelled getter label value -> do
      histogram <- asks getter
      unsafeEff_ $ P.withLabel histogram label (`P.observe` value)

increaseCounter :: Metrics metricType :> es => (metricType -> P.Counter) -> Eff es ()
increaseCounter getter = send (IncreaseCounter getter)

setGauge :: Metrics metricType :> es => (metricType -> P.Gauge) -> Double -> Eff es ()
setGauge getter value = send (SetGauge getter value)

increaseLabelledCounter
  :: (P.Label label, Metrics metricType :> es)
  => (metricType -> P.Vector label P.Counter)
  -> label
  -> Eff es ()
increaseLabelledCounter getter label = send (IncreaseLabelledCounter getter label)

setLabelledGauge :: (P.Label label, Metrics metricType :> es) => (metricType -> P.Vector label P.Gauge) -> label -> Double -> Eff es ()
setLabelledGauge getter label value = send (SetLabelledGauge getter label value)

observe :: Metrics metricType :> es => (metricType -> P.Histogram) -> Double -> Eff es ()
observe getter value = send (Observe getter value)

observeLabelled
  :: (P.Label label, Metrics metricType :> es)
  => (metricType -> P.Vector label P.Histogram)
  -> Double
  -> label
  -> Eff es ()
observeLabelled getter label value = send (ObserveLabelled getter value label)
