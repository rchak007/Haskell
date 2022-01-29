

data BowlerType = Fast | MediumFast | LegSpinner | OffSpinner


type AvgRunsPerWicket = Integer
type TotalWickets = Integer
type Stats = (AvgRunsPerWicket, TotalWickets)
type BowlerWickets = (Name, TotalWickets)
type Name = String
type TeamName = String

data Bowler = Bowler Name BowlerType Stats  -- Bowler has Name, type, stats


data Country = String

tournament :: [Team]    -- Tournament has multiple teams


data Team = Team TeamName Bowler     --- Each Team has Team name and a Bowler


-- so we get from Team to Bowler and then Bowler to stats.
teamAussie = Team "Australia" (Bowler "Waugh" MediumFast (28, 75))

tournament = 
    [ Team "West Indies" (Bowler "Joel Garner" Fast (22, 156)) , 
      Team "India" (Bowler "Kapil Dev" Fast (23, 266)), 
      Team "Sri Lanka" (Bowler "Muralitharan" LegSpinner (25, 300)), 
      Team "Australia" (Bowler "Waugh" MediumFast (28, 75))]

getBowlerFromTeam :: Team -> Bowler
getBowlerFromTeam (Team _ c) = c

--  This section we just run through the teams and return just the Total wickets for each team
getTotalWicketsFromBowler :: Bowler -> TotalWickets
getTotalWicketsFromBowler (Bowler _ _ (_,d)) = d

getTotalWicketsFromTeam :: Team -> TotalWickets
getTotalWicketsFromTeam = getTotalWicketsFromBowler . getBowlerFromTeam      --- Team -> Bowler then Bowler -> Totalwicket


assessBowlers :: [Team] -> [Integer]
assessBowlers []     = []
assessBowlers (x:xs) = getTotalWicketsFromTeam x : assessBowlers xs


--   Below we return Tuple (Bowlername , Total wickets) for each team

getBowlerNameWicketsFromBowler :: Bowler -> BowlerWickets
getBowlerNameWicketsFromBowler (Bowler n _ (_,w)) = (n, w)

getBowlerNameWicketsFromTeam :: Team -> BowlerWickets
getBowlerNameWicketsFromTeam = getBowlerNameWicketsFromBowler . getBowlerFromTeam

assessBowlersNameWickets :: [Team] -> [BowlerWickets]
assessBowlersNameWickets [] = []
assessBowlersNameWickets (x:xs) = getBowlerNameWicketsFromTeam x : assessBowlersNameWickets xs    -- --- Team -> Bowler then Bowler -> (Name, Totalwicket)




--- Instead now using map
assessBowlersNameWickets' :: [Team] -> [BowlerWickets]
assessBowlersNameWickets' = map getBowlerNameWicketsFromTeam

assessedBowlersNameWickets'' :: [BowlerWickets]
assessedBowlersNameWickets'' = assessBowlersNameWickets' tournament



main :: IO ()
main = do
    putStrLn ("****** Aussies total wickets = " ++ show(getTotalWicketsFromTeam teamAussie))
    putStrLn ("*** " ++ show(assessBowlers tournament))
    putStrLn ("*** Using Recursion " ++ show(assessBowlersNameWickets tournament))
    putStrLn ("**** Using Map " ++ show(assessedBowlersNameWickets''))