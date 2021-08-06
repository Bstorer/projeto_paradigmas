-- Always imported
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random


type Coord = (Float,Float)
type Life = Float
type Vel = (Float,Float)
type ShowObj = Bool

data Enemie = MkBigEnemie Picture Coord Vel ShowObj Life | MkMiniEnemie Picture Coord Vel ShowObj
data Player = MkPlayer Picture Coord Vel
data Shot   = MkShot Picture Coord Vel ShowObj
data World  = MkWorld Player [Enemie] [Shot]

limiteX, limiteY :: Num a => a
limiteX = 400
limiteY = 300

drawShot:: Shot -> Picture
drawShot (MkShot p (x,y) v so) = translate x y p

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
drawingWorld (MkWorld p enemies shots) = pictures [pic_p, pic_enemies, pic_shots]
    where
        pic_p = drawingPlayer p
        pic_enemies = drawingEnemies enemies
        pic_shots = drawingShots shots

updatePosShot :: Float -> Shot -> Shot
updatePosShot dt (MkShot p (x,y) (vx,vy) so) = MkShot p (x + vx*dt,y + vy*dt) (vx,vy) so

updateShowShot ::  Shot -> Shot
updateShowShot (MkShot p (x,y) v so)
    | x > limiteX || x < -limiteX || y > limiteY || y < -limiteY = MkShot p (x,y) v False
    | otherwise = MkShot p (x,y) v True

shotNeedShow :: Shot -> Bool
shotNeedShow (MkShot p (x,y) v so) = so

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
    | y > limiteY =  MkBigEnemie p (x,limiteY) (vx,-vy) so l
    | y < -limiteY = MkBigEnemie p (x,-limiteY) (vx,-vy) so l
    | otherwise  = MkBigEnemie p (x,y) (vx,vy) so l
updateVelEnemie (MkMiniEnemie p (x,y) (vx,vy) so)
    | x > limiteX  = MkMiniEnemie p (limiteX,y) (-vx,vy) so
    | x < -limiteX = MkMiniEnemie p (-limiteX,y) (-vx,vy) so 
    | y > limiteY =  MkMiniEnemie p (x,limiteY) (vx,-vy) so 
    | y < -limiteY = MkMiniEnemie p (x,-limiteY) (vx,-vy) so 
    | otherwise  = MkMiniEnemie p (x,y) (vx,vy) so 

updateEnemies :: Float -> [Enemie] -> [Enemie]
updateEnemies dt enemies = map (updateVelEnemie . updatePosEnemie dt) enemies

verifyShotEnemie :: Shot -> Enemie -> Bool
verifyShotEnemie (MkShot sp (sx,sy) sv sso)  (MkBigEnemie ep (ex,ey) ev eso l) = getShot
    where 
        dist = sqrt ((sx - ex)*(sx - ex) + (sy - ey)*(sy - ey))
        getShot 
            | dist <= 50 = True 
            | otherwise = False
verifyShotEnemie (MkShot sp (sx,sy) sv sso)  (MkMiniEnemie ep (ex,ey) ev eso) = getShot
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
updateShotShow  nEnemies (MkShot p (x,y) v so)
    | nEnemies <= 0 = MkShot p (x,y) v True 
    | otherwise = MkShot p (x,y) v False 

enemiesShotsInteraction:: [Enemie] -> Shot -> Shot
enemiesShotsInteraction enemies shot = shotAfterEnemie
    where
        shotsEnemiesGotHit = [enemie | enemie <- enemies, verifyShotEnemie shot enemie]
        nEnemies = length shotsEnemiesGotHit
        shotAfterEnemie = updateShotShow nEnemies shot

updateEnemiesAndShots :: Float -> [Enemie] -> [Shot] -> ([Enemie],[Shot])
updateEnemiesAndShots dt enemies shots = (enimesUpdated,shotsUpdated)
    where
        enemiesAfterShots = map (shotsEnemiesInteraction shots) enemies
        enemiesToShow = [enemie | enemie <- enemiesAfterShots, enemieNeedShow enemie]
        enimesUpdated = updateEnemies dt enemiesToShow
        shotsAfterEnemies = map (enemiesShotsInteraction enemies) shots
        shotsToShow = [shot | shot <- shotsAfterEnemies, shotNeedShow shot]
        shotsUpdated = updateShots dt shotsToShow



updatePlayer :: Float -> Player -> Player 
updatePlayer _ (MkPlayer p (x,y) v)
    | x > limiteX  = MkPlayer p (limiteX,y) v
    | x < -limiteX = MkPlayer p (-limiteX,y) v
    | y > limiteY =  MkPlayer p (x,limiteY) v
    | y < -limiteY = MkPlayer p (x,-limiteY) v 
    | otherwise  = MkPlayer p (x,y) v

getEnemiesFromTuple :: ([Enemie],[Shot]) -> [Enemie]
getEnemiesFromTuple (enemies,shots) = enemies 

getShotsFromTuple :: ([Enemie],[Shot]) -> [Shot]
getShotsFromTuple (enemies,shots) = shots

updateWorld :: Float ->  World -> World
updateWorld dt (MkWorld p enemies shots) = MkWorld (updatePlayer dt p) enemiesUpdated shotsUpdated
    where
        enemiesAndShotsUpdated = updateEnemiesAndShots dt enemies shots
        enemiesUpdated = getEnemiesFromTuple enemiesAndShotsUpdated
        shotsUpdated = getShotsFromTuple enemiesAndShotsUpdated

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

getShotFormat :: Picture
getShotFormat = color white (circleSolid 5)

addShot :: Player -> Coord -> [Shot] -> [Shot]
addShot (MkPlayer p (x,y) v) (xPos,yPos)  shots = shots ++ [newShot]
    where
        baseVel = 100
        angDegree = getMouseAng (xPos,yPos) (x,y)
        xv = baseVel * (sin angDegree)
        yv = baseVel * (cos angDegree)
        newShot = MkShot getShotFormat (x,y+30) (xv,yv) True

inputHandler :: Event -> World -> World
inputHandler (EventKey (SpecialKey keyPressed) Down _ (xPos,yPos)) (MkWorld p enemies shots) = 
    case (keyPressed) of
        KeyRight -> MkWorld (moveThePlayer xPos yPos 10 p) enemies shots
        KeyLeft  -> MkWorld (moveThePlayer xPos yPos (-10) p) enemies shots
        KeySpace -> MkWorld (moveThePlayer xPos yPos 0 p) enemies (addShot p (xPos,yPos) shots)
        _ -> MkWorld (moveThePlayer xPos yPos 0 p) enemies shots
inputHandler (EventMotion (xPos,yPos)) (MkWorld p enemies shots) = MkWorld (moveThePlayer xPos yPos 0 p) enemies shots
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
        enemies = createEnemies g  5
        player = MkPlayer (getPlayerShip 0) (0,-80) (0,0)
        initWorld = MkWorld player enemies []
        
    play
       displayWindow
       black
       simulationRate
       initWorld
       drawingWorld
       inputHandler
       updateWorld
   
        

