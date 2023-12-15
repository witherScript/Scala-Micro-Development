object MyAssembly:
  private def formatResult(name: String, n: Int, f:Int => Int) =
    val msg = "the %s of %d is %d"
    msg.format(name, n, factorial(n))
  
  def factorial(x: Int): Int = 
    def go (x: Int, acc: Int): Int =
      if x <= 1 then acc
      else go(x-1, acc*x)
    go(x, 1)

  //Ex. 2.2
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (gt(as(n), as(n + 1))) false
      else loop(n + 1)
    }
    loop(0)
     

  @main def printFac: Unit =
    println(formatResult("factorial", 4, factorial))
    println("Should be true: ")
    println(isSorted(Array(1,2,3), _ > _))
    println(isSorted(Array(3,2,1), _ < _))
    println("Should be false: ")
    println(isSorted(Array(3,2,1), _ < _))