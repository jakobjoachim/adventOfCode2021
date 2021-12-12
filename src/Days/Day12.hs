module Days.Day12 where

import Data.Char (isUpper)
import Data.List (nub)
import Data.Tuple (swap)

type Point = String

type Path = (Point, Point)

type Route = [Point]

runTask :: [Path] -> Int
runTask xs =
  let paths = xs ++ map swap xs
   in length $ nextWaypoints (filter (\(x,y) -> x /= "start" && y /= "start") paths) $ getStarts paths

getStarts :: [Path] -> [Route]
getStarts xs = [[y, x] | (x, y) <- xs, x == "start"]

nextWaypoints :: [Path] -> [Route] -> [Route]
nextWaypoints _ rs | all (\xs -> head xs == "end") rs = rs
nextWaypoints p rs                                    = nextWaypoints p $ concat [nextWaypointsForRoute p r | r <- rs]

-- if the Route is a dead end return [] (drops the route
nextWaypointsForRoute :: [Path] -> Route -> [Route]
nextWaypointsForRoute _ r@("end":_) = [r]
nextWaypointsForRoute p r           = let neighbours = filterValidNeighbours r (allNeighbouringPoints p (head r))
                                      in if null neighbours then [] else [ x : r | x <- neighbours]

allNeighbouringPoints :: [Path] -> Point -> [Point]
allNeighbouringPoints allPaths currentPoint = [to | (from, to) <- allPaths, from == currentPoint]

filterValidNeighbours :: Route -> [Point] -> [Point]
filterValidNeighbours r = filter (\x -> all isUpper x || x `notElem` r || noSmallCaveTwice r)

noSmallCaveTwice :: Route -> Bool
noSmallCaveTwice r = let smallCavesInRoute = filter (not . all isUpper) r
                      in length smallCavesInRoute == length (nub smallCavesInRoute)
