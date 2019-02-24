module Particle where
import Prelude
import Math (sqrt)
import Data.List

type Tick = Number

type TupleXY = {
  x :: Number,
  y :: Number
}

type Accel = TupleXY

type Particle = {
  v :: TupleXY,
  p :: TupleXY,
  collisionRadius :: Number
}


nextTick :: Particle -> Accel -> Tick -> Particle
nextTick e a t = e {
    p = nextPosition e t,
    v = nextVelocity e a t
  }

nextPosition :: Particle -> Tick -> TupleXY
nextPosition e t = {
    x: newX,
    y: newY
  }
  where newX = (e.v.x * t + e.p.x) :: Number
        newY = (e.v.y * t + e.p.y) :: Number

nextVelocity :: Particle -> Accel -> Tick -> TupleXY
nextVelocity e a t = {
    x: newX,
    y: newY
  }
  where newX = (a.x * t + e.v.x) :: Number
        newY = (a.y * t + e.v.y) :: Number

detectCollision :: List Particle -> Particle -> Boolean
detectCollision Nil b = false
detectCollision (a:as) b = collide a b || detectCollision as b

collide :: Particle -> Particle -> Boolean
collide e1 e2 = d > sqrt ((e1.p.x * e2.p.x) + (e1.p.y * e2.p.y))
  where d = e1.collisionRadius + e2.collisionRadius
