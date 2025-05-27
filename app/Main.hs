{-#Language DuplicateRecordFields#-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Algebra.Graph
import Graphics.Vega.VegaLite as VL
import Data.Map.Strict as Map
import qualified Data.Text as T


main :: IO()
main = regionData (dataFromRows [] (makeDataSet 0 regionMap))

--Time Unit is one week
data Region = Region {alive :: Double, zombie :: Double, dead :: Double, birthRate :: Double, naturalDeathRate :: Double, exodusRate :: Double} deriving (Eq, Ord)
instance Show Region where
    show :: Region -> String
    show region = "alive: " ++ show (alive region) ++ " zombie: " ++ show (zombie region) ++ " dead: " ++ show (dead region)
data ZombieSpeciesConstants = ZombieSpeciesConstants {zombificationRate :: Double, zombieDeathRate :: Double, reanimationRate :: Double}

regionMap :: Map String Region
regionMap = fromList [("A", a), ("B", b), ("C", c), ("D", d)]
    where
    a = Region{
        alive = 10000,
        zombie = 50,
        dead = 0,
        birthRate = 100,
        naturalDeathRate = 0.001,
        exodusRate = 0.001
    }
    b = Region{
       alive = 10000,
        zombie = 200,
        dead = 0,
        birthRate = 10,
        naturalDeathRate = 0.01,
        exodusRate = 0.001
    }
    c = Region{
        alive = 200,
        zombie = 5,
        dead = 0,
        birthRate = 10,
        naturalDeathRate = 0.01,
        exodusRate = 0.001
    }
    d = Region{
        alive = 10000,
        zombie = 50,
        dead = 0,
        birthRate = 10,
        naturalDeathRate = 0.01,
        exodusRate = 0.3
    }

regionConnections :: Graph String
regionConnections = edges [(a,b), (b, a), (b,c), (c,b), (c,a), (a,c), (a,d), (d,a), (b,d), (d,b)]
    where
        a = "A"
        b = "B"
        c = "C"
        d = "D"

step :: Double
step = 0.01

maxTime :: Double
maxTime = 100.0

constants :: ZombieSpeciesConstants
constants = ZombieSpeciesConstants{
    zombificationRate = 0.00003,
    zombieDeathRate = 0.00006,
    reanimationRate = 0.02
}
regionData :: Data -> IO()
regionData zombieData = toHtmlFile "./regions.html" $ toVegaLite [zombieData, hConcat [specA, specB, specC, specD]]
    where
        aliveA = [mark Line [MColor "blue"], encoding (position Y [PName "Alive A", PmType Quantitative] [])]
        zombieA = [mark Line [MColor "green"], encoding (position Y [PName "Zombie A", PmType Quantitative] [])]
        deadA = [mark Line [MColor "gray"], encoding (position Y [PName "Dead A", PmType Quantitative] [])]
        specA =  asSpec [encoding (position X [PName "Time", PmType Quantitative] []), layer (Prelude.map asSpec [aliveA, zombieA, deadA])]

        aliveB = [mark Line [MColor "blue"], encoding (position Y [PName "Alive B", PmType Quantitative] [])]
        zombieB = [mark Line [MColor "green"], encoding (position Y [PName "Zombie B", PmType Quantitative] [])]
        deadB = [mark Line [MColor "gray"], encoding (position Y [PName "Dead B", PmType Quantitative] [])]
        specB = asSpec [encoding (position X [PName "Time", PmType Quantitative] []), layer (Prelude.map asSpec [aliveB, zombieB, deadB])]

        aliveC = [mark Line [MColor "blue"], encoding (position Y [PName "Alive C", PmType Quantitative] [])]
        zombieC = [mark Line [MColor "green"], encoding (position Y [PName "Zombie C", PmType Quantitative] [])]
        deadC = [mark Line [MColor "gray"], encoding (position Y [PName "Dead C", PmType Quantitative] [])]
        specC = asSpec [encoding (position X [PName "Time", PmType Quantitative] []), layer (Prelude.map asSpec [aliveC, zombieC, deadC])]

        aliveD = [mark Line [MColor "blue"], encoding (position Y [PName "Alive D", PmType Quantitative] [])]
        zombieD = [mark Line [MColor "green"], encoding (position Y [PName "Zombie D", PmType Quantitative] [])]
        deadD = [mark Line [MColor "gray"], encoding (position Y [PName "Dead D", PmType Quantitative] [])]
        specD =  asSpec [encoding (position X [PName "Time", PmType Quantitative] []), layer (Prelude.map asSpec [aliveD, zombieD, deadD])]


makeDataSet :: Double -> Map.Map String Region ->  [DataRow]
makeDataSet time dataMap =
    if time < maxTime
    then
        dataRow (("Time", Number time) :  concatMap regionAsData (vertexList regionConnections)) (makeDataSet (time + step) mapAdvanced)
    else
        []
    where
        mapAdvanced = updateMap (edgeList regionConnections) dataMap
        regionAsData :: String -> [(FieldName, DataValue)]
        regionAsData regionName = [(T.pack $ "Alive " ++ regionName, Number $ alive region), (T.pack $ "Zombie " ++ regionName, Number $ zombie region), (T.pack $ "Dead " ++ regionName, Number $ dead region)]
            where region = mapAdvanced Map.! regionName



updateMap :: [(String, String)] -> Map.Map String Region -> Map.Map String Region
updateMap ((k1, k2) : connections) regions = updateMap connections adjustedMap
    where
        adjustedMap = Map.adjust (const $ fst mUpdate) k2 (Map.adjust (const $ snd mUpdate) k1 regions)
        mUpdate = migrationUpdate (regions Map.! k2, regions Map.! k1)
updateMap [] regions = Map.map regionUpdate regions

migrationUpdate :: (Region, Region) -> (Region, Region)
migrationUpdate (leaveRegion, enterRegion) = ((leaveRegion {alive = alive leaveRegion - migrationNumber}), (enterRegion {alive = alive enterRegion + migrationNumber}))
    where
        migrationNumber = min (exodusRate leaveRegion * alive leaveRegion * (zombie leaveRegion / zombie enterRegion) * step)  (alive leaveRegion)

regionUpdate :: Region -> Region
regionUpdate region = region {
    alive = alive region + dAlive,
    zombie = zombie region + dZombie,
    dead = dead region + dDead
    }
    where
        dAlive = (births - naturalDeaths - zombification) * step
        dZombie = (zombification - zombieKills + reanimation) * step
        dDead = (naturalDeaths + zombieKills - reanimation) * step
        births = if alive region > 0.0 then birthRate region else 0.0
        naturalDeaths = naturalDeathRate region * alive region
        zombification = zombificationRate constants * alive region * zombie region
        zombieKills = zombieDeathRate constants * zombie region * alive region
        reanimation = reanimationRate constants * dead region