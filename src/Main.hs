-- Always imported
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random


type Coord = (Float,Float)
type Life = Float
type Vel = (Float,Float)
type ShowObj = Bool

data Enemie = MkBigEnemie Picture Coord Vel Life | MkMiniEnemie Picture Coord Vel
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
drawEnemie (MkBigEnemie p (x,y) v l) = translate x y p
drawEnemie (MkMiniEnemie p (x,y) v) = translate x y p

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

updateShots :: Float -> [Shot] -> [Shot]
updateShots dt shots = newShotsFiltered
    where 
        newShots = map (updateShowShot . updatePosShot dt) shots 
        newShotsFiltered = [shot | shot <- newShots, shotNeedShow shot]


updatePosEnemie :: Float -> Enemie -> Enemie
updatePosEnemie dt (MkBigEnemie p (x,y) (vx,vy) l) = MkBigEnemie p (x + vx*dt,y + vy*dt) (vx,vy) l
updatePosEnemie dt (MkMiniEnemie p (x,y) (vx,vy))  = MkMiniEnemie p (x + vx*dt,y + vy*dt) (vx,vy)

updateVelEnemie ::  Enemie -> Enemie
updateVelEnemie (MkBigEnemie p (x,y) (vx,vy) l)
    | x > limiteX  = MkBigEnemie p (limiteX,y) (-vx,vy) l
    | x < -limiteX = MkBigEnemie p (-limiteX,y) (-vx,vy) l
    | y > limiteY =  MkBigEnemie p (x,limiteY) (vx,-vy) l
    | y < -limiteY = MkBigEnemie p (x,-limiteY) (vx,-vy) l
    | otherwise  = MkBigEnemie p (x,y) (vx,vy) l
updateVelEnemie (MkMiniEnemie p (x,y) (vx,vy))
    | x > limiteX  = MkMiniEnemie p (limiteX,y) (-vx,vy) 
    | x < -limiteX = MkMiniEnemie p (-limiteX,y) (-vx,vy) 
    | y > limiteY =  MkMiniEnemie p (x,limiteY) (vx,-vy) 
    | y < -limiteY = MkMiniEnemie p (x,-limiteY) (vx,-vy) 
    | otherwise  = MkMiniEnemie p (x,y) (vx,vy) 




updateEnemies :: Float -> [Enemie] -> [Enemie]
updateEnemies dt enemies = map (updateVelEnemie . updatePosEnemie dt) enemies 



updatePlayer :: Float -> Player -> Player 
updatePlayer _ (MkPlayer p (x,y) v)
    | x > limiteX  = MkPlayer p (limiteX,y) v
    | x < -limiteX = MkPlayer p (-limiteX,y) v
    | y > limiteY =  MkPlayer p (x,limiteY) v
    | y < -limiteY = MkPlayer p (x,-limiteY) v 
    | otherwise  = MkPlayer p (x,y) v

updateWorld :: Float ->  World -> World
updateWorld dt (MkWorld p enemies shots) = MkWorld (updatePlayer dt p) (updateEnemies dt enemies) (updateShots dt shots)



createEnemie :: (Float,Float) -> (Float,Float) -> Int -> Enemie
createEnemie coord vel 1 = MkBigEnemie  (getShipFormat red  180 1.2 1.2) coord vel 30
createEnemie coord vel 0 = MkMiniEnemie (getShipFormat blue 180 0.8 0.8) coord vel
createEnemie coord vel _ = MkMiniEnemie (getShipFormat blue 180 0.8 0.8) coord vel

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
   
        

