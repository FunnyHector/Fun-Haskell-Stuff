module Metrics (
    MetricUnit(..),
    ImperialUnit(..),
    Measurement(..),
    convert )
where

data Unit = MetricUnit
          | ImperialUnit
            deriving (Show)

data MetricUnit = Meter
                | Liter
                | KiloGram
                  deriving (Show, Eq)

data ImperialUnit = Yard
                  | Gallon
                  | Pound
                    deriving (Show, Eq)

data Measurement = MetricMeasurement { amount::Double, munit::MetricUnit }
                 | ImperialMeasurement { amount::Double, iunit::ImperialUnit }
                   deriving (Show)

convert :: Measurement -> Measurement
convert (MetricMeasurement x u)
    | u == Meter    = ImperialMeasurement (1.0936 * x) Yard
    | u == Liter    = ImperialMeasurement (0.2642 * x) Gallon
    | u == KiloGram = ImperialMeasurement (2.2046 * x) Pound
convert (ImperialMeasurement x u)
    | u == Yard     = MetricMeasurement (0.9144 * x) Meter
    | u == Gallon   = MetricMeasurement (3.7854 * x) Liter
    | u == Pound    = MetricMeasurement (0.4536 * x) KiloGram

-- symbol :: MetricUnit -> String
-- symbol Meter = "m"
-- symbol Liter = "L"
-- symbol KiloGram = "kg"
-- symbol u | u == Meter    = "m"
--          | u == Liter    = "L"
--          | u == KiloGram = "kg"
