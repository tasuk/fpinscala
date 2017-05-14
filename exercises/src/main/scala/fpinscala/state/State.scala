package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def mapWithFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s) { i => unit(f(i)) }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (int, rng2) = rng.nextInt
    (if (int < 0) -(int + 1) else int, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (int, rng2) = nonNegativeInt(rng)
    (int.toDouble / (Int.MaxValue + 1), rng2)
  }

  val doubleWithMap: Rand[Double] =
    map(nonNegativeInt)(_.toDouble / (Int.MaxValue + 1))

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (int, rng2) = nonNegativeInt(rng)
    val (dbl, rng3) = double(rng2)
    ((int, dbl), rng3)
  }
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (int, rng2) = nonNegativeInt(rng)
    val (dbl, rng3) = double(rng2)
    ((dbl, int), rng3)
  }
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (dbl1, rng2) = double(rng)
    val (dbl2, rng3) = double(rng2)
    val (dbl3, rng4) = double(rng3)
    ((dbl1, dbl2, dbl3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count > 0) {
      val (i, r) = rng.nextInt
      val (l: List[Int], r2: RNG) = ints(count - 1)(r)
      (i :: l, r2)
    } else {
      (List(), rng)
    }
  }

  def intsWithSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng1 => {
      val (a, rng2) = ra(rng1)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(aa => map(rb)(bb => f(aa, bb)))

  // List[RNG => (A, RNG)]: RNG => (List[A], RNG)
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((el, acc) => map2(el, acc)(_ :: _))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
}

import State._

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    })
}

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List()))((el, acc) => el.map2(acc)(_ :: _))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))

  // type Rand[A] = State[RNG, A]
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin+1)
      case (Turn, Machine(false, candy, coin)) => Machine(true, candy-1, coin)
      case _ => s
    }

  def simulateMachine(input: Input): State[Machine, (Int, Int)] =
    for {
      _ <- (modify[Machine](_)).compose(update)(input)
      s <- get
    } yield (s.coins, s.candies)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- sequence(inputs.map((modify[Machine](_)).compose(update)))
      s <- get
    } yield (s.coins, s.candies)

  //def asdf =
  //  simulateMachine(List(Coin, Turn, Coin, Turn)).run(Machine(false, 3, 3))
}
