-- Always imported
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random

type Coord = (Float,Float)
type Life = Float
type Vel = (Float,Float)
type ShowObj = Bool
type GameScore = Int
type Time = Int
type RestarTime = Int  
type BaseTime = Int
type Status = Bool
type PowerType = String


data Star   = MkStar Picture Coord Vel
data Power  = MkPower Picture Coord Vel [PowerType] Time | NoPower deriving (Eq,Show)
data Game   = MkGame Time RestarTime BaseTime GameScore Power Status
data Enemie = MkBigEnemie Picture Coord Vel ShowObj Life | MkMiniEnemie Picture Coord Vel ShowObj
data Player = MkPlayer Picture Coord Vel
data Shot   = MkShot Picture Coord Vel ShowObj Power deriving Eq
data World  = MkWorld Game Player [Enemie] [Shot] [Power] [Star]


class Draw a where
    draw :: a -> Picture

instance Draw Star where 
    draw (MkStar p (x,y) v) = translate x y p

instance Draw Power where 
    draw (MkPower p (x,y) v powerTypes time) = translate x y p
    draw NoPower = Blank

instance Draw Shot where 
    draw (MkShot p (x,y) v so power) = translate x y p

instance Draw Enemie where 
    draw (MkBigEnemie p (x,y) v so l) =  enemieShip
        where 
            enemieShip
                    | l >= 30 = translate x y p
                    | l >= 20 = translate x y (getEnemieShip (makeColor (210/255) (96/255) (8/255) 1) 1.2)
                    | l >= 10 = translate x y (getEnemieShip (makeColor (210/255) (62/255) (8/255) 1) 1.2)
                    | otherwise = translate x y p

    draw (MkMiniEnemie p (x,y) v so) = translate x y p

instance Draw Player where 
    draw (MkPlayer p (x,y) v)= translate x y p

instance Semigroup Power where
    NoPower <> NoPower = NoPower
    (MkPower p coord vel powersTypes time) <> NoPower = MkPower p coord vel powersTypes time
    NoPower <> (MkPower p coord vel powersTypes time) = MkPower p coord vel powersTypes time
    (MkPower p1 coord1 vel1 powersTypes1 time1) <> (MkPower p2 coord2 vel2 powersTypes2 time2) = 
        MkPower Blank (0,0)  (0,0) (powersTypes1 ++ powersTypes2) (time1 + time2)
   
  
instance Monoid Power where
    mempty  = NoPower



fold :: Monoid a => [a] -> a
fold []  = mempty
fold (x:xs) = x `mappend` fold xs


limiteX, limiteY :: Num a => a
limiteX = 400
limiteY = 300

drawingStars :: [Star] -> Picture
drawingStars stars = pictures (map draw stars)

drawingPowers :: [Power] -> Picture
drawingPowers powers = pictures (map draw powers)

drawingShots :: [Shot] -> Picture
drawingShots shots = pictures (map draw shots)


drawingEnemies :: [Enemie] -> Picture
drawingEnemies enemies = pictures (map draw enemies)


getFirstShot :: [Shot] -> Shot
getFirstShot [] = MkShot Blank (0,0) (0,0) False NoPower
getFirstShot shots = shots!!0

getGamePower :: Power -> Int 
getGamePower (MkPower _ _ _ _ time) = time
getGamePower _ = -1

drawingWorld :: World -> Picture
drawingWorld (MkWorld (MkGame time restart baseTime points gamePower True) player enemies shots powers stars) = pictures [pic_stars, pic_player, pic_enemies, pic_shots, pic_powers, textPoints]
    where
        pic_player = draw player
        pic_enemies = drawingEnemies enemies
        pic_shots = drawingShots shots
        pic_powers = drawingPowers powers
        pic_stars = drawingStars stars

        pointsStr = show points
        textPoints = translate x y  (scale s s (color white (text ("Pontos: " ++ pointsStr))))
            where
                x::Float
                y::Float 
                x = -400
                y = 250
                s = 0.3

drawingWorld (MkWorld (MkGame time restart baseTime points power False) p enemies shots powers stars) = textPoints
    where
        pointsStr = show points
        textPoints = translate x y  (scale s s (color white (text ("Fim de jogo! Pontos totais: " ++ pointsStr))))
            where
                x::Float
                y::Float 
                x = -300
                y = 0
                s = 0.3


updatePosShot :: Float -> Shot -> Shot
updatePosShot dt (MkShot p (x,y) (vx,vy) so power) = MkShot p (x + vx*dt,y + vy*dt) (vx,vy) so power

updateShowShot ::  Shot -> Shot
updateShowShot (MkShot p (x,y) v so power)
    | x > limiteX || x < -limiteX || y > limiteY || y < -limiteY = MkShot p (x,y) v False power
    | otherwise = MkShot p (x,y) v True power

shotNeedShow :: Shot -> Bool
shotNeedShow (MkShot p (x,y) v so power) = so

enemieNeedShow :: Enemie -> Bool
enemieNeedShow (MkBigEnemie p (x,y) v so l) = so
enemieNeedShow (MkMiniEnemie p (x,y) v so) = so


updateShots :: Float -> [Shot] -> [Shot]
updateShots dt shots = newShotsFiltered
    where 
        newShots = map (updateShowShot . updatePosShot dt) shots 
        newShotsFiltered = [shot | shot <- newShots, shotNeedShow shot]


updatePosEnemie :: Float -> Enemie -> Enemie
updatePosEnemie dt (MkBigEnemie p (x,y) (vx,vy) so l) = MkBigEnemie p (x + vx*dt,y + vy*dt) (vx,vy) so l
updatePosEnemie dt (MkMiniEnemie p (x,y) (vx,vy) so)  = MkMiniEnemie p (x + vx*dt,y + vy*dt) (vx,vy) so

updateVelEnemie ::  Enemie -> Enemie
updateVelEnemie (MkBigEnemie p (x,y) (vx,vy) so l)
    | x > limiteX  = MkBigEnemie p (limiteX,y) (-vx,vy) so l
    | x < -limiteX = MkBigEnemie p (-limiteX,y) (-vx,vy) so l
    | y < -limiteY = MkBigEnemie p (x,-limiteY) (vx,-vy) so l
    | otherwise  = MkBigEnemie p (x,y) (vx,vy) so l
updateVelEnemie (MkMiniEnemie p (x,y) (vx,vy) so)
    | x > limiteX  = MkMiniEnemie p (limiteX,y) (-vx,vy) so
    | x < -limiteX = MkMiniEnemie p (-limiteX,y) (-vx,vy) so 
    | y < -limiteY = MkMiniEnemie p (x,-limiteY) (vx,-vy) so 
    | otherwise  = MkMiniEnemie p (x,y) (vx,vy) so 

updateEnemies :: Float -> [Enemie] -> [Enemie]
updateEnemies dt enemies = map (updateVelEnemie . updatePosEnemie dt) enemies


verifyPlayerPower :: Player -> Power -> Bool
verifyPlayerPower (MkPlayer pPlayer (xPlayer,yPlayer) vPlayer)  (MkPower pPower (xPower,yPower) vPower powersTypes powerTime) = getPower
    where 
        dist = sqrt ((xPlayer - xPower)*(xPlayer - xPower) + (yPlayer - yPower)*(yPlayer - yPower))
        getPower 
            | dist <= 40 = True 
            | otherwise = False

verifyShotEnemie :: Shot -> Enemie -> Bool
verifyShotEnemie (MkShot sp (sx,sy) sv sso power)  (MkBigEnemie ep (ex,ey) ev eso l) = getShot
    where 
        dist = sqrt ((sx - ex)*(sx - ex) + (sy - ey)*(sy - ey))
        getShot 
            | dist <= 50 = True 
            | otherwise = False
verifyShotEnemie (MkShot sp (sx,sy) sv sso power)  (MkMiniEnemie ep (ex,ey) ev eso) = getShot
    where 
        dist = sqrt ((sx - ex)*(sx - ex) + (sy - ey)*(sy - ey))
        getShot 
            | dist <= 40 = True 
            | otherwise = False

updateEnemieShow :: Int -> Enemie -> Enemie
updateEnemieShow  nShots (MkBigEnemie p (x,y) v so l)
    | nShots <= 0 = MkBigEnemie p (x,y) v True l
    | l - 10 > 0 = MkBigEnemie p (x,y) v True (l - 10)
    | otherwise = MkBigEnemie p (x,y) v False 0
updateEnemieShow  nShots (MkMiniEnemie p (x,y) v so) 
    | nShots <= 0 = MkMiniEnemie p (x,y) v True 
    | otherwise = MkMiniEnemie p (x,y) v False 

shotsEnemiesInteraction ::  [Shot] -> Enemie -> Enemie
shotsEnemiesInteraction shots enemie = enemieAfterShot
    where
        shotsThatHitEnemie = [shot | shot <- shots, verifyShotEnemie shot enemie]
        nShots = length shotsThatHitEnemie
        enemieAfterShot = updateEnemieShow nShots enemie

updateShotShow :: Int -> Shot -> Shot
updateShotShow  nEnemies (MkShot p (x,y) v so power)
    | nEnemies <= 0 = MkShot p (x,y) v True power
    | otherwise = MkShot p (x,y) v False power


getEnemieType :: Enemie -> Int
getEnemieType  (MkBigEnemie p (x,y) v so l) = 1
getEnemieType  (MkMiniEnemie p (x,y) v so) = 0


getShotPower :: Shot -> [PowerType]
getShotPower (MkShot pShot (x,y) v so (MkPower pPower posPower vPower powersTypes powerTime) ) = powersTypes
getShotPower (MkShot pShot (x,y) v so NoPower ) = []



enemiesShotsInteraction:: [Enemie] -> Shot -> Shot
enemiesShotsInteraction enemies shot = shotAfterEnemie
    where
        powerTypes = getShotPower shot
        shotAfterEnemie
            | (elem "Imortal" powerTypes) = shot 
            | otherwise = shotAfterEnemieValue
                where 
                    shotsEnemiesGotHit = [enemie | enemie <- enemies, verifyShotEnemie shot enemie]
                    nEnemies = length shotsEnemiesGotHit
                    shotAfterEnemieValue = updateShotShow nEnemies shot

-- separeteShotsAndPoints :: [(Shot,Int)] -> ([Shot],Int)
-- separeteShotsAndPoints shotsPoints
--     | shotsPoints == [] = ([],0)
--     | otherwise = ([shot] ++ shotsTail, points + pointsTail)
--         where
--             (shot,points) = head shotsPoints
--             (shotsTail,pointsTail) = separeteShotsAndPoints (tail shotsPoints)

updateEnemiesShotsGame :: Float -> Game -> [Enemie] -> [Shot] -> (Game,[Enemie],[Shot])
updateEnemiesShotsGame dt (MkGame time restart baseTime points power True) enemies shots = (gameUpdated,enemiesUpdated,shotsUpdated)
    where
        enemiesAfterShots = map (shotsEnemiesInteraction shots) enemies
        enemiesToShow = [enemie | enemie <- enemiesAfterShots, enemieNeedShow enemie]
        enemiesUpdated = updateEnemies dt enemiesToShow

        shotsAfterEnemies = map (enemiesShotsInteraction enemies) shots
        shotsToShow = [shot | shot <- shotsAfterEnemies, shotNeedShow shot]
        shotsUpdated = updateShots dt shotsToShow

        enemiesToNotShow = [enemie | enemie <- enemiesAfterShots, not (enemieNeedShow enemie)]
        nMiniEnemiesHit = length [enemie | enemie <- enemiesToNotShow, (getEnemieType enemie) == 0]
        nBigEnemiesHit = length [enemie | enemie <- enemiesToNotShow, (getEnemieType enemie) == 1]
        pointsGot = 20*nBigEnemiesHit + 10*nMiniEnemiesHit

        gameUpdated = updateGame (MkGame time restart baseTime points power True) pointsGot enemiesUpdated

updateEnemiesShotsGame dt (MkGame time restart baseTime points power False) enemies shots = (gameUpdated,[],[])
    where
        gameUpdated = updateGame (MkGame time restart baseTime points power False) 0 []

updateStar :: Float -> Star -> Star 
updateStar dt (MkStar p (x,y) (xv,yv))
    | x > limiteX  = MkStar p (-limiteX,y) (xv,yv)
    | x < -limiteX = MkStar p (limiteX,y) (xv,yv)
    | y > limiteY =  MkStar p (x,-limiteY) (xv,yv)
    | y < -limiteY = MkStar p (x,limiteY) (xv,yv) 
    | otherwise  = MkStar p (x + dt*xv,y + dt*yv) (xv,yv)


updateStars :: Float -> [Star] -> [Star]
updateStars dt stars = map (updateStar dt) stars

updatePlayer :: Player -> Player 
updatePlayer (MkPlayer p (x,y) v)
    | x > limiteX  = MkPlayer p (limiteX,y) v
    | x < -limiteX = MkPlayer p (-limiteX,y) v
    | y > limiteY =  MkPlayer p (x,limiteY) v
    | y < -limiteY = MkPlayer p (x,-limiteY) v 
    | otherwise  = MkPlayer p (x,y) v

updatePower :: Float -> Power -> Power
updatePower dt (MkPower p (x,y) (xv,yv) powersTypes powerTime) =MkPower p (x + xv*dt,y + yv*dt) (xv,yv) powersTypes powerTime

updateGamePower :: Power -> Power 
updateGamePower NoPower = NoPower
updateGamePower (MkPower p (x,y) (xv,yv) powersTypes powerTime) 
    | powerTime > 0 = MkPower p (x,y) (xv,yv) powersTypes (powerTime - 1)
    | otherwise = NoPower

updatePlayerPowersGame :: Float -> Player -> [Power] -> Game -> (Player,[Power],Game)
updatePlayerPowersGame dt player powers (MkGame time restart baseTime points power True) = (updatedPlayer,updatedPowers,updatedGame)
    where
        updatedPlayer = updatePlayer player
        powersToShow = [power | power <- powers, not(verifyPlayerPower player power)]
        updatedPowers = map (updatePower dt) powersToShow

        gamePowerUpdated = updateGamePower power
        powersEarned = fold [power | power <- powers, verifyPlayerPower player power]
        updatedGame = MkGame time restart baseTime points (powersEarned <> gamePowerUpdated) True

    
updatePlayerPowersGame dt player powers (MkGame time restart baseTime points power False) = (updatedPlayer,updatedPowers,updatedGame)
    where
        updatedPlayer = MkPlayer (getPlayerShip 0) (0,-230) (0,0)
        updatedPowers = []  
        updatedGame = MkGame time restart baseTime points power False

getEnemiesFromTuple :: ([Enemie],[Shot]) -> [Enemie]
getEnemiesFromTuple (enemies,shots) = enemies 

getShotsFromTuple :: ([Enemie],[Shot]) -> [Shot]
getShotsFromTuple (enemies,shots) = shots

checkEnemiePosition :: Enemie -> Bool
checkEnemiePosition  (MkBigEnemie p (x,y) v so l) = y <= -limiteY
checkEnemiePosition   (MkMiniEnemie p (x,y) v so) = y <= -limiteY 

updateGame :: Game -> Int -> [Enemie] -> Game
updateGame (MkGame time restart baseTime points power True) pointsGot enemies = MkGame (time + 1) newRestarTime baseTime (points + pointsGot) power newStatus
    where
        enemiesWin = [enemie | enemie <- enemies, checkEnemiePosition enemie ]    
        nEnemiesWin = length enemiesWin
        newStatus
            | nEnemiesWin > 0 = False
            | otherwise = True
        newRestarTime
            | nEnemiesWin > 0 = 0
            | otherwise = restart

updateGame (MkGame time restart baseTime points power False) pointsGot enemies = MkGame time (restart + 1) newBaseTime newPoints NoPower newStatus
    where
        newBaseTime = time
        newStatus
            | restart > 300 = True
            | otherwise = False
        newPoints
            | restart > 300 = 0
            | otherwise = points



getNewPower :: StdGen -> Game -> [Power]
getNewPower g (MkGame time restart baseTime points power status) = newPower
    where 
        (gT,gPV) = split g
        (gP,gV) = split gPV
        xP = limiteX - 2*(limiteX * ((randomRs (0::Float,1::Float) gP)!!time))
        yVBase = (-200) * ((randomRs (0::Float,1::Float) gV)!!time)
        yV
            | yVBase > -50 = -50
            | otherwise = yVBase
        coord = (xP,380)
        vel = (0,yV)
        powerType :: Float
        powerType = ((randomRs (0::Float,1::Float) gT)!!time)
        factor = (log (fromIntegral (100*(time-baseTime)) :: Float))
        newPower 
            | powerType <= 0.00003 * factor = [createPower coord vel 0] 
            | powerType <= 0.00007 * factor = [createPower coord vel 1] 
            | powerType <= 0.0001 * factor  = [createPower coord vel 2] 
            | otherwise = []

getNewEnemie :: StdGen -> Game -> [Enemie]
getNewEnemie g (MkGame time restart baseTime points power status) = newEnemie
    where 
        (gT,gPV) = split g
        (gP,gV) = split gPV
        xP = limiteX - 2*(limiteX * ((randomRs (0::Float,1::Float) gP)!!time))
        yVBase = (-200) * ((randomRs (0::Float,1::Float) gV)!!time)
        yV
            | yVBase > -50 = -50
            | otherwise = yVBase
        coord = (xP,380)
        vel = (0,yV)
        enemType :: Float
        enemType = ((randomRs (0::Float,1::Float) gT)!!time)
        factor = (log (fromIntegral (100*(time-baseTime)) :: Float))
        newEnemie
            | enemType <= 0.0005 * factor = [createEnemie coord vel 1] 
            | enemType <= 0.001 * factor = [createEnemie coord vel 0]
            | otherwise = []

getGameTime :: Game -> Time 
getGameTime (MkGame time restart baseTime points power status) = time

getGamePoints :: Game -> GameScore 
getGamePoints (MkGame time restart baseTime points power status) = points


updateWorld :: StdGen -> Float -> World -> World
updateWorld g dt (MkWorld game player enemies shots powers stars) = MkWorld gameUpdated2 playerUpdated enemiesAll shotsUpdated powersAll updatedStars
    where
        (gEnemies, gPowers) = split g
        (gameUpdated1,enemiesUpdated,shotsUpdated) = updateEnemiesShotsGame dt game enemies shots
        time = getGameTime gameUpdated1

        (playerUpdated,powerUpdated,gameUpdated2) = (updatePlayerPowersGame dt player powers gameUpdated1)

        updatedStars = updateStars dt stars

        newEnemie = getNewEnemie gEnemies game
        enemiesAll = enemiesUpdated ++ newEnemie

        newPower = getNewPower gPowers game
        powersAll = powerUpdated ++ newPower 


        

getEnemieShip:: Color -> Float -> Picture
getEnemieShip c scale = getShipFormat1 c 180 scale scale

createEnemie :: (Float,Float) -> (Float,Float) -> Int -> Enemie
createEnemie coord vel 1  = MkBigEnemie  (getEnemieShip (makeColor r g b 1) 1.2) coord vel True 30
    where
        r::Float
        g::Float
        b::Float
        r = 232/255
        g = 136/255
        b = 12/255

createEnemie coord vel 0  = MkMiniEnemie (getEnemieShip (makeColor r g b 1) 0.8) coord vel True
    where
        r::Float
        g::Float
        b::Float
        r = 72/255
        g = 135/255
        b = 243/255


getStarFormat :: Picture
getStarFormat = color starColor (circleSolid 2)
    where  
        starColor = makeColor 255 255 255 0.5

createStar :: Coord -> Star
createStar pos = MkStar (getStarFormat) pos (50,50)

createStarsPositions :: Int -> Coord -> [Coord]
createStarsPositions 0 _ = []
createStarsPositions n (lastX,lastY)
    | lastY < -limiteY = []
    | (lastX + s) >= limiteX  = [(-limiteX, lastY - s)] ++ (createStarsPositions (n - 1) (-limiteX,lastY - s))
    | otherwise = [(lastX + s, lastY)] ++ (createStarsPositions (n - 1) (lastX + s,lastY))
        where
            s = 50

createStars :: [Star]
createStars = map createStar (createStarsPositions 500 (-limiteX, limiteY))

getMouseAng ::  Coord -> Coord -> Float
getMouseAng (xPos,yPos) (pX,pY) = aTang
    where
        aTang = atan ((xPos - pX)/(yPos - pY))


getMouseAngDegree ::  Coord -> Coord -> Float
getMouseAngDegree (xPos,yPos) (pX,pY) = angDegree
    where
        aTang = getMouseAng (xPos,yPos) (pX,pY)
        angDegree = (180/pi) * aTang

moveThePlayer :: Float -> Float -> Float -> Player -> Player
moveThePlayer xPos yPos mv (MkPlayer p (x,y) v) = MkPlayer newPic (newX,y) v
    where
        newX = x + mv
        angDegree = getMouseAngDegree (xPos,yPos) (newX,y)
        adjustedAng
            | angDegree > 45  = 45
            | angDegree < -45 = -45
            | otherwise = angDegree
        newPic = getPlayerShip adjustedAng


createPower :: Coord -> Vel -> Int -> Power 
createPower coord vel 0 =  (MkPower (color (makeColor (196/255) (225/255) (11/255) 1) (circleSolid 10)) coord vel ["Imortal"] 300)
createPower coord vel 1 =  (MkPower (color (makeColor (11/255) (203/255) (225/255) 1) (circleSolid 10)) coord vel ["Trio"] 300)
createPower coord vel 2 =  (MkPower (color (makeColor (6/255) (208/255) (73/255) 1) (circleSolid 10)) coord vel ["SuperSpeed"] 300)


getShotFormat :: Power -> Picture
getShotFormat NoPower = color white (circleSolid 5)
getShotFormat (MkPower p coord vel powerTypes time)
    | (elem "Imortal" powerTypes) = color yellow (circleSolid 5)
    | otherwise = color white (circleSolid 5)

getPowerTypes :: Power -> [String]
getPowerTypes NoPower = []
getPowerTypes (MkPower p coord vel powersTypes time) = powersTypes


createShotWithAngle :: Float -> Float -> Power -> Float -> Shot
createShotWithAngle x y power angle = MkShot (getShotFormat power) (x,y+30) (xv,yv) True power
    where
        powerTypes = getPowerTypes power
        baseVel
            | (elem "SuperSpeed" powerTypes) = 900
            | otherwise = 300
        xv = baseVel * (sin angle)
        yv = baseVel * (cos angle)
        

addShot :: Game -> Player -> Coord -> [Shot] -> [Shot]
addShot (MkGame time restart baseTime score power status) (MkPlayer p (x,y) v) (xPos,yPos)  shots = shots ++ newShots
    where
        baseAngle = 0.785398
        powerTypes = getPowerTypes power
        angDegree = getMouseAng (xPos,yPos) (x,y)
        adjustedAng
            | angDegree > baseAngle = baseAngle 
            | angDegree < -baseAngle  = -baseAngle
            | otherwise = angDegree
        newShots
            | (elem "Trio" powerTypes) = map (createShotWithAngle x y power) [adjustedAng - baseAngle , adjustedAng, adjustedAng + baseAngle]
            | otherwise = [createShotWithAngle x y power adjustedAng]

inputHandler :: Event -> World -> World

inputHandler (EventKey (MouseButton _) Down _ (xPos,yPos)) (MkWorld game p enemies shots powers stars) = 
    MkWorld game (moveThePlayer xPos yPos 0 p) enemies (addShot game p (xPos,yPos) shots) powers stars


inputHandler (EventKey (SpecialKey keyPressed) Down _ (xPos,yPos)) (MkWorld game p enemies shots powers stars) = 
    case (keyPressed) of
        KeyRight -> MkWorld game (moveThePlayer xPos yPos 60 p) enemies shots powers stars
        KeyLeft  -> MkWorld game (moveThePlayer xPos yPos (-60) p) enemies shots powers stars
        KeySpace -> MkWorld game (moveThePlayer xPos yPos 0 p) enemies (addShot game p (xPos,yPos) shots) powers stars
        _ -> MkWorld game (moveThePlayer xPos yPos 0 p) enemies shots powers stars



inputHandler (EventMotion (xPos,yPos)) (MkWorld game p enemies shots powers stars) = MkWorld game (moveThePlayer xPos yPos 0 p) enemies shots powers stars
inputHandler _ w = w




getShipFormat1 :: Color -> Float -> Float -> Float -> Picture
getShipFormat1 c r x y = scale x y (color c (rotate r ship))
    where 
         ship = polygon [
                    (  0,  50),
                    ( 40,  20),
                    ( 20,   0),
                    ( 30, -30),
                    (  0, -10),
                    (-30, -30),
                    (-20,   0),
                    (-40,  20),
                    (  0,  50)
                ]



getShipFormat2 :: Color -> Float -> Float -> Float -> Picture
getShipFormat2 c r x y = scale x y (color c (rotate r ship))
    where 
        ship = polygon [
            (  0, 50),
            ( 30, 40),
            ( 40, 20),
            ( 50,  0),
            ( 30,-30),
            (  0,-30),
            (-30,-30),
            (-50,  0),
            (-40, 20),
            (-30, 40),
            (  0, 50)
            ]

getPlayerShip :: Float -> Picture
getPlayerShip ang = getShipFormat2 (makeColor r g b 1) ang 0.75 0.75
    where
        r::Float
        g::Float
        b::Float
        r = 176/255
        g = 173/255
        b = 179/255




main :: IO()
main = do 
    g <- getStdGen
    let displayWindow = InWindow "Enemies atack" (2 * limiteX, 2 * limiteY) (50, 50)
        simulationRate = 60
        stars = createStars
        player = MkPlayer (getPlayerShip 0) (0,-230) (0,0)
        game = MkGame 0 0 0 0 NoPower True 
        initWorld = MkWorld game player [] [] [] stars
        
    play
       displayWindow
       black
       simulationRate
       initWorld
       drawingWorld
       inputHandler
       (updateWorld g)
   
        

