module Entity where
  import Particle
  

  data Entity =
    Kinematic
    | Dynamic Particle