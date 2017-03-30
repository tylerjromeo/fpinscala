// Use this as a scala worksheet or repl commands to test out the rng functions
import fpinscala.state.RNG
import fpinscala.state.RNG._
val rng = RNG.Simple(1258888)
rng.nextInt
nonNegativeInt(rng)
double(rng)
intDouble(rng)
doubleInt(rng)
double3(rng)
ints(9)(rng)
ints(1)(rng)