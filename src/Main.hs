-- Always imported
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random


type Coord = (Float,Float)
type Life = Float
type Vel = (Float,Float)


data Enemie = MkBigEnemie Picture Coord Vel Life | MkMiniEnemie Picture Coord Vel
data Player = MkPlayer Picture Coord Vel
data World  = MkWorld Player [Enemie]

limiteX, limiteY :: Num a => a
limiteX = 400
limiteY = 300

drawEnemie:: Enemie -> Picture
drawEnemie (MkBigEnemie p (x,y) v l) = translate x y p
drawEnemie (MkMiniEnemie p (x,y) v) = translate x y p

drawingEnemies :: [Enemie] -> Picture
drawingEnemies enemies = pictures (map drawEnemie enemies)


drawingPlayer :: Player -> Picture 
drawingPlayer (MkPlayer p (x,y) v)= translate x y p

drawingWorld :: World -> Picture
drawingWorld (MkWorld p enemies) = pictures [pic_p, pic_enemies]
    where
        pic_p = drawingPlayer p
        pic_enemies = drawingEnemies enemies



updatePos :: Float -> Enemie -> Enemie
updatePos dt (MkBigEnemie p (x,y) (vx,vy) l) = MkBigEnemie p (x + vx*dt,y + vy*dt) (vx,vy) l
updatePos dt (MkMiniEnemie p (x,y) (vx,vy))  = MkMiniEnemie p (x + vx*dt,y + vy*dt) (vx,vy)

updateVel ::  Enemie -> Enemie
updateVel (MkBigEnemie p (x,y) (vx,vy) l)
    | x > limiteX  = MkBigEnemie p (limiteX,y) (-vx,vy) l
    | x < -limiteX = MkBigEnemie p (-limiteX,y) (-vx,vy) l
    | y > limiteY =  MkBigEnemie p (x,limiteY) (vx,-vy) l
    | y < -limiteY = MkBigEnemie p (x,-limiteY) (vx,-vy) l
    | otherwise  = MkBigEnemie p (x,y) (vx,vy) l
updateVel (MkMiniEnemie p (x,y) (vx,vy))
    | x > limiteX  = MkMiniEnemie p (limiteX,y) (-vx,vy) 
    | x < -limiteX = MkMiniEnemie p (-limiteX,y) (-vx,vy) 
    | y > limiteY =  MkMiniEnemie p (x,limiteY) (vx,-vy) 
    | y < -limiteY = MkMiniEnemie p (x,-limiteY) (vx,-vy) 
    | otherwise  = MkMiniEnemie p (x,y) (vx,vy) 




updateEnemies :: Float -> [Enemie] -> [Enemie]
updateEnemies dt enemies = map (updateVel . updatePos dt) enemies 



updatePlayer :: Float -> Player -> Player 
updatePlayer _ (MkPlayer p (x,y) v)
    | x > limiteX  = MkPlayer p (limiteX,y) v
    | x < -limiteX = MkPlayer p (-limiteX,y) v
    | y > limiteY =  MkPlayer p (x,limiteY) v
    | y < -limiteY = MkPlayer p (x,-limiteY) v 
    | otherwise  = MkPlayer p (x,y) v

updateWorld :: Float ->  World -> World
updateWorld dt (MkWorld p enemies) = MkWorld (updatePlayer dt p) (updateEnemies dt enemies)



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


moveThePlayer :: Float -> Float -> Float -> Player -> Player
moveThePlayer xPos yPos mv (MkPlayer p (x,y) v) = MkPlayer newPic (newX,y) v
    where
        newX = x + mv
        aTang = atan ((xPos - newX)/(yPos - y))
        angDegree = (180/pi) * aTang
        newPic = getPlayerShip angDegree


inputHandler :: Event -> World -> World
inputHandler (EventKey (SpecialKey keyPressed) Down _ (xPos,yPos)) (MkWorld p enemies) = 
    case (keyPressed) of
        KeyRight -> MkWorld (moveThePlayer xPos yPos 10 p) enemies
        KeyLeft  -> MkWorld (moveThePlayer xPos yPos (-10) p) enemies
        _ -> MkWorld (moveThePlayer xPos yPos 0 p) enemies
inputHandler (EventMotion (xPos,yPos)) (MkWorld p enemies) = MkWorld (moveThePlayer xPos yPos 0 p) enemies
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
        initWorld = MkWorld player enemies
        
    play
       displayWindow
       white
       simulationRate
       initWorld
       drawingWorld
       inputHandler
       updateWorld
   
        

