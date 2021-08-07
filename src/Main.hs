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
type Status = Bool
type PowerType = String

data Power  = MkPower Picture Coord Vel [PowerType] Time deriving Eq
data Game   = MkGame Time GameScore Status
data Enemie = MkBigEnemie Picture Coord Vel ShowObj Life | MkMiniEnemie Picture Coord Vel ShowObj
data Player = MkPlayer Picture Coord Vel
data Shot   = MkShot Picture Coord Vel ShowObj Power deriving Eq
data World  = MkWorld Game Player [Enemie] [Shot] [Power]

limiteX, limiteY :: Num a => a
limiteX = 400
limiteY = 300


drawPower :: Power -> Picture 
drawPower (MkPower p (x,y) v powerTypes time) = translate x y p

drawingPowers :: [Power] -> Picture
drawingPowers powers = pictures (map drawPower powers)

drawShot:: Shot -> Picture
drawShot (MkShot p (x,y) v so power) = translate x y p

drawingShots :: [Shot] -> Picture
drawingShots shots = pictures (map drawShot shots)

drawEnemie:: Enemie -> Picture
drawEnemie (MkBigEnemie p (x,y) v so l) =  translate x y (getEnemieShip enemieColor 1.2)
    where 
        enemieColor
           | l >= 30 = green--makeColor 82 4 161 1
           | l >= 20 = yellow --makeColor 6 3 98 1
           | l >= 10 = red --makeColor 55 3 98 1
           | otherwise = white
        
drawEnemie (MkMiniEnemie p (x,y) v so) = translate x y p


drawingEnemies :: [Enemie] -> Picture
drawingEnemies enemies = pictures (map drawEnemie enemies)


drawingPlayer :: Player -> Picture 
drawingPlayer (MkPlayer p (x,y) v)= translate x y p

drawingWorld :: World -> Picture
drawingWorld (MkWorld (MkGame time points True) player enemies shots powers) = pictures [pic_player, pic_enemies, pic_shots, pic_powers, textPoints]
    where
        pic_player = drawingPlayer player
        pic_enemies = drawingEnemies enemies
        pic_shots = drawingShots shots
        pic_powers = drawingPowers powers

        pointsStr = show (length powers) -- points
        textPoints = translate x y  (scale s s (color white (text ("Pontos: " ++ pointsStr))))
            where
                x::Float
                y::Float 
                x = -400
                y = 250
                s = 0.3

drawingWorld (MkWorld (MkGame time points False) p enemies shots powers) = textPoints
    where
        pointsStr = show points
        textPoints = translate x y  (scale s s (color white (text ("Fim de jogo!\n Pontos totais: " ++ pointsStr))))
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
            | dist <= 30 = True 
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

enemiesShotsInteraction:: [Enemie] -> Shot -> Shot
enemiesShotsInteraction enemies shot = shotAfterEnemie
    where
        powers = getShotPower shot
        shotAfterEnemie
            | (elem "Test" powers) = shot 
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
updateEnemiesShotsGame dt (MkGame time points True) enemies shots = (gameUpdated,enemiesUpdated,shotsUpdated)
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

        gameUpdated = updateGame (MkGame time points True) pointsGot enemiesUpdated

updateEnemiesShotsGame dt (MkGame time points False) enemies shots = (gameUpdated,[],[])
    where
        gameUpdated = updateGame (MkGame time points False) 0 []


updatePlayer :: Player -> Player 
updatePlayer (MkPlayer p (x,y) v)
    | x > limiteX  = MkPlayer p (limiteX,y) v
    | x < -limiteX = MkPlayer p (-limiteX,y) v
    | y > limiteY =  MkPlayer p (x,limiteY) v
    | y < -limiteY = MkPlayer p (x,-limiteY) v 
    | otherwise  = MkPlayer p (x,y) v

updatePower :: Float -> Power -> Power
updatePower dt (MkPower p (x,y) (xv,yv) powersTypes powerTime) =MkPower p (x + xv*dt,y + yv*dt) (xv,yv) powersTypes powerTime

updatePlayerPowers :: Float -> Player -> [Power] -> Game -> (Player,[Power])
updatePlayerPowers dt player powers (MkGame time points True) = (updatedPlayer,updatedPowers)
    where
        updatedPlayer = updatePlayer player
        powersToShow = [power | power <- powers, not(verifyPlayerPower player power)]
        updatedPowers = map (updatePower dt) powersToShow
    
updatePlayerPowers dt player powers (MkGame time points False) = (updatedPlayer,updatedPowers)
    where
        updatedPlayer = MkPlayer (getPlayerShip 0) (0,-230) (0,0)
        updatedPowers = []  

getEnemiesFromTuple :: ([Enemie],[Shot]) -> [Enemie]
getEnemiesFromTuple (enemies,shots) = enemies 

getShotsFromTuple :: ([Enemie],[Shot]) -> [Shot]
getShotsFromTuple (enemies,shots) = shots

checkEnemiePosition :: Enemie -> Bool
checkEnemiePosition  (MkBigEnemie p (x,y) v so l) = y <= -limiteY
checkEnemiePosition   (MkMiniEnemie p (x,y) v so) = y <= -limiteY 

updateGame :: Game -> Int -> [Enemie] -> Game
updateGame (MkGame time points True) pointsGot enemies = MkGame newTime (points + pointsGot) newStatus
    where
        enemiesWin = [enemie | enemie <- enemies, checkEnemiePosition enemie ]    
        nEnemiesWin = length enemiesWin
        newStatus
            | nEnemiesWin > 0 = False
            | otherwise = True
        newTime
            | nEnemiesWin > 0 = 0
            | otherwise = time + 1

updateGame (MkGame time points False) pointsGot enemies = MkGame (time + 1) newPoints newStatus
    where
        newStatus
            | time > 300 = True
            | otherwise = False
        newPoints
            | time > 300 = 0
            | otherwise = points

sigmoid :: Float -> Float
sigmoid x = 1/(1 + (2.718)**(-x))


getNewPower :: StdGen -> Time -> [Power]
getNewPower g time = newPower
    where 
        (gT,gPV) = split g
        (gP,gV) = split gPV
        xP = limiteX - 2*(limiteX * ((randomRs (0::Float,1::Float) gP)!!time))
        yV = (-200) * ((randomRs (0::Float,1::Float) gV)!!time)
        coord = (xP,360)
        vel = (0,yV)
        powerType :: Float
        powerType = ((randomRs (0::Float,1::Float) gT)!!time)
        factor = (log (fromIntegral (100*time) :: Float))
        newPower 
            | powerType <= 0.0005 * factor = [createPower coord vel] 
            | otherwise = []

getNewEnemie :: StdGen -> Time -> [Enemie]
getNewEnemie g time = newEnemie
    where 
        (gT,gPV) = split g
        (gP,gV) = split gPV
        xP = limiteX - 2*(limiteX * ((randomRs (0::Float,1::Float) gP)!!time))
        yV = (-200) * ((randomRs (0::Float,1::Float) gV)!!time)
        coord = (xP,360)
        vel = (0,yV)
        enemType :: Float
        enemType = ((randomRs (0::Float,1::Float) gT)!!time)
        factor = (log (fromIntegral (100*time) :: Float))
        newEnemie
            | enemType <= 0.0005 * factor = [createEnemie coord vel 1] 
            | enemType <= 0.001 * factor = [createEnemie coord vel 0]
            | otherwise = []

getGameTime :: Game -> Time 
getGameTime (MkGame time points status) = time

getGamePoints :: Game -> GameScore 
getGamePoints (MkGame time points status) = points


updateWorld :: StdGen -> Float -> World -> World
updateWorld g dt (MkWorld game player enemies shots powers) = MkWorld gameUpdated playerUpdated enemiesAll shotsUpdated powersAll
    where
        (gEnemies, gPowers) = split g
        (gameUpdated,enemiesUpdated,shotsUpdated) = updateEnemiesShotsGame dt game enemies shots
        time = getGameTime gameUpdated

        (playerUpdated,powerUpdated) = (updatePlayerPowers dt player powers game)

        newEnemie = getNewEnemie gEnemies time
        enemiesAll = enemiesUpdated ++ newEnemie

        newPower = getNewPower gPowers time
        powersAll = powerUpdated ++ newPower 


        

getEnemieShip:: Color -> Float -> Picture
getEnemieShip c scale = getShipFormat c 180 scale scale

createEnemie :: (Float,Float) -> (Float,Float) -> Int -> Enemie
createEnemie coord vel 1  = MkBigEnemie  (getEnemieShip red 1.2) coord vel True 30
createEnemie coord vel 0  = MkMiniEnemie (getEnemieShip blue 0.8) coord vel True
createEnemie coord vel _  = MkMiniEnemie (getEnemieShip blue 0.8) coord vel True

createEnemies :: StdGen -> Int -> [Enemie]
createEnemies g n = take n $ zipWith3 createEnemie coords vels tps 
    where 
        (g1, g2) = split g
        (xGen, yGen) = split g1
        (gTp, gVel) = split g2

   
        getVel :: Float -> Vel
        getVel r = (0 , 200 * (sin r))
        vels = map (\x -> getVel x ) (randoms gVel)

        convertCoord :: Float -> Float -> Coord
        convertCoord x y = (2*x*limiteX - limiteX,30)
        coords = zipWith convertCoord (randoms xGen) (randoms yGen)

        getTp :: Float -> Int
        getTp c = if c <= 0.5
            then 0
            else 1
        
        calculateTp :: Float -> Int
        calculateTp f = getTp (sin f)
        
        tps :: [Int]
        tps = map calculateTp (randoms gTp)
    

        --cores = cycle [red,black,yellow,blue,green]


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


createPower :: Coord -> Vel -> Power 
createPower coord vel =  (MkPower (color red (circleSolid 20)) coord vel ["Test2"] 0)

getShotFormat :: Picture
getShotFormat = color white (circleSolid 5)

addShot :: Player -> Coord -> [Shot] -> [Shot]
addShot (MkPlayer p (x,y) v) (xPos,yPos)  shots = shots ++ [newShot]
    where
        baseVel = 300
        angDegree = getMouseAng (xPos,yPos) (x,y)
        adjustedAng
            | angDegree > 0.785398  = 0.785398 
            | angDegree < -0.785398  = -0.785398 
            | otherwise = angDegree
        xv = baseVel * (sin adjustedAng)
        yv = baseVel * (cos adjustedAng)
        newShot = MkShot getShotFormat (x,y+30) (xv,yv) True (MkPower (color red (circleSolid 5)) (0,0) (0,0) ["Test2"] 0)

inputHandler :: Event -> World -> World

inputHandler (EventKey (MouseButton _) Down _ (xPos,yPos)) (MkWorld game p enemies shots powers) = 
    MkWorld game (moveThePlayer xPos yPos 0 p) enemies (addShot p (xPos,yPos) shots) powers


inputHandler (EventKey (SpecialKey keyPressed) Down _ (xPos,yPos)) (MkWorld game p enemies shots powers) = 
    case (keyPressed) of
        KeyRight -> MkWorld game (moveThePlayer xPos yPos 30 p) enemies shots powers
        KeyLeft  -> MkWorld game (moveThePlayer xPos yPos (-30) p) enemies shots powers
        KeySpace -> MkWorld game (moveThePlayer xPos yPos 0 p) enemies (addShot p (xPos,yPos) shots) powers
        _ -> MkWorld game (moveThePlayer xPos yPos 0 p) enemies shots powers



inputHandler (EventMotion (xPos,yPos)) (MkWorld game p enemies shots powers) = MkWorld game (moveThePlayer xPos yPos 0 p) enemies shots powers
inputHandler _ w = w




getShipFormat :: Color -> Float -> Float -> Float -> Picture
getShipFormat c r x y = scale x y (color c (rotate r ship))
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

getPlayerShip :: Float -> Picture
getPlayerShip ang = getShipFormat green ang 0.75 0.75


main :: IO()
main = do 
    g <- getStdGen
    let displayWindow = InWindow "Enemies atack" (2 * limiteX, 2 * limiteY) (50, 50)
        simulationRate = 60
        player = MkPlayer (getPlayerShip 0) (0,-230) (0,0)
        game = MkGame 300 0 True
        initWorld = MkWorld game player [] [] []
        
    play
       displayWindow
       black
       simulationRate
       initWorld
       drawingWorld
       inputHandler
       (updateWorld g)
   
        

