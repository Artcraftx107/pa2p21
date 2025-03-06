object practica21 {
  //Ejercicio 1
 def factoresPrimos(n: Int): List[Int] = {
   def factorizar(num: Int, divisor: Int, acc: List[Int]): List[Int] = {
     if(num == 1){
       acc.reverse
     }else if(num%divisor == 0){
       factorizar(num/divisor, divisor, divisor :: acc)
     }else{
       factorizar(num, divisor + 1, acc)
     }

   }
   factorizar(n, 2, Nil)
 } //Fin ejercicio 1

  //Ejercicio 2
  def binarySearch(arr: Array[Int], elt: Int): Option[Int] ={
    def buscar(i: Int, j: Int): Option[Int] = {
      if(i>j){
        return None
      }else{
        val mid = i + (j-i)/2
        if(arr(mid) == elt){
          Some(mid)
        }else if(arr(mid)>elt){
          buscar(i, mid-1)
        }else{
          buscar(mid+1, j)
        }
      }
    }
    buscar(0, arr.length)
  } //Fin ejercicio 2

  //Ejercicio 3
  def unzip[A, B](list: List[(A, B)]): (List[A], List[B]) = {
    def unzipper(resultado: List[(A, B)], accA: List[A], accB: List[B]): (List[A], List[B]) = resultado match{
      case Nil => (accA.reverse, accB.reverse)
      case(a, b) :: tail => unzipper(tail, a :: accA, b :: accB)
    }
    unzipper(list, Nil, Nil)
  }

  //Ejercicio 4
  def zip[A, B](list1: List[A], list2: List[B]): List[(A, B)] ={
    def zipper(l1: List[A], l2: List[B], acc: List[(A, B)]): List[(A, B)]= (l1, l2) match{
      case (Nil, _)|(_, Nil) => acc.reverse
      case (h1 :: t1, h2 :: t2)=>zipper(t1, t2, (h1, h2) :: acc)
    }
    zipper(list1, list2, Nil)
  }

  //Ejercicio 5
  def filtro[A](list: List[A], f: A => Boolean): List[A] = {
    def auxiliar(resto: List[A], acc: List[A]): List[A] = resto match{
      case Nil => acc.reverse
      case h :: t => if(f(h)) auxiliar(t, h :: acc) else auxiliar(t, acc)
    }
    auxiliar(list, Nil)
  }

  //Ejercicio 6
  def map[A, B](list: List[A], f: A => B): List[B] = {
    def auxiliar(resto: List[A], acc: List[B]): List[B] = resto match{
      case Nil => acc.reverse
      case h :: t => auxiliar(t, f(h) :: acc)
    }
    auxiliar(list, Nil)
  }

  //Ejercicio 7
  def group[A, B](list: List[A], f: A => B): Map[B, List[A]] = {
    def auxiliar(resto: List[A], acc: Map[B, List[A]]): Map[B, List[A]] = resto match{
      case Nil => acc
      case h :: t =>
        val key = f(h)
        val listaActualizada = acc.getOrElse(key, Nil) :+ h
        auxiliar(t, acc + (key -> listaActualizada))
    }
    auxiliar(list, Map.empty)
  }

 def main(args: Array[String]): Unit = {
   println("Resultado ejercicio 1: ")
   println(factoresPrimos(60)) // Output: List(2, 2, 3, 5)
   println(factoresPrimos(97)) // Output: List(97)
   println(factoresPrimos(84)) // Output: List(2, 2, 3, 7)
   println("---------------------------------------------------------------------")
   println("Resultado ejercicio 2: ")
   val arr = Array(1, 3, 5, 7, 9, 11)
   println(binarySearch(arr, 5)) // Output: Some(2)
   println(binarySearch(arr, 10)) // Output: None
   println("---------------------------------------------------------------------")
   println("Resultado ejercicio 3: ")
   println(unzip(List((10, 'a'), (20, 'b'), (30, 'c')))) // Output: (List(10, 20, 30), List('a', 'b', 'c'))
   println("---------------------------------------------------------------------")
   println("Resultado ejercicio 4: ")
   println(zip(List(10, 20, 30), List('a', 'b', 'c'))) // Output: List((10, 'a'), (20, 'b'), (30, 'c'))
   println(zip(List(10, 20, 30), List('a', 'b'))) // Output: List((10, 'a'), (20, 'b'))
   println("---------------------------------------------------------------------")
   println("Resultado ejercicio 5: ")
   println(filtro(List(1,2,3,4,5), _ % 2 == 0)) // Output: List(2,4)
   println("---------------------------------------------------------------------")
   println("Resultado ejercicio 6: ")
   println(map(List(1,2,3,4,5), _ * 2)) // Output: List(2,4,6,8,10)
   println("---------------------------------------------------------------------")
   println("Resultado ejercicio 7: ")
   println(group(List(1,2,3,4,5), _ % 2 == 0)) // Output: Map(false -> List(1, 3, 5), true -> List(2, 4))
 }
}