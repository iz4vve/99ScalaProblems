def testAnswer(answer: Boolean): String =  

    if (answer == true) "OK"
    else "WRONG!!!"
// answer match {
//     case true => "OK"
//     case _ => "WRONG!!!"
// }

//////////////////////////////////////////////////////////////////////////
// P01
// (*) Find the last element of a list.
// Example:
// scala> last(List(1, 1, 2, 3, 5, 8))
// res0: Int = 8
val P01 = List(1, 1, 2, 3, 5, 8)

def last[T](list: List[T]): T = {
    // list(list.length - 1)
    //this is built-in already...
    list.last
}

val answer01 = last(P01)
val answerTest01 = testAnswer(answer01 == 8)

println(f"P01: $answer01 ($answerTest01)")


//////////////////////////////////////////////////////////////////////////
// P02
// Find the last but one element of a list.
// Example:
// scala> penultimate(List(1, 1, 2, 3, 5, 8))
// res0: Int = 5
val P02 = List(1, 1, 2, 3, 5, 8)

def penultimate[T](list: List[T]): T = list match {
    // list(list.length - 2)   // this will throw exceptions in case list is empty or list.length < 2
    // let's go recursive...
    // case h :: Nil => h // this finds the tail,  not the second last
    case h :: _ :: Nil => h  // this adds an element before Nil
    case h :: t => penultimate(t)
    case _ => throw new NoSuchElementException
}

val answer02 = penultimate(P02)
val answerTest02 = testAnswer(answer02 == 5)

println(f"P02: $answer02 ($answerTest02)")


//////////////////////////////////////////////////////////////////////////
// P03 
// (*) Find the Kth element of a list.
// By convention, the first element in the list is element 0.
// Example:

// scala> nth(2, List(1, 1, 2, 3, 5, 8))
// res0: Int = 2
val P03 = List(1, 1, 2, 3, 5, 8)

def nth[T](n: Int, list: List[T]): T =  (n, list) match {
    // if using builtins, this is trivial
    // if (n >= 0) ls(n)
    // else throw new NoSuchElementException

    // recursive
    case (0, h :: _) => h   // get the element when n goes to zero
    case (n, h :: t) => nth(n - 1, t)  // decreases n and apply to tail
    case (_, Nil) => throw new NoSuchElementException // not enough elements in list
    
}

val answer03 = nth(2, P03)
val answerTest03 = testAnswer(answer03 == 2)

println(f"P03: $answer03 ($answerTest03)")


//////////////////////////////////////////////////////////////////////////
// P04 
// (*) Find the number of elements of a list.
// Example:
// scala> length(List(1, 1, 2, 3, 5, 8))
// res0: Int = 6
val P04 = List(1, 1, 2, 3, 5, 8)

def length[T](list: List[T]): Int = {

    // trivial with builtins
    // list.size
    // list.length

    // foldLeft
    // list.foldLeft(0)((it, _) => it + 1)

    // recursive
    list match {
        case Nil => 0
        case h :: t => 1 + length(t)
    }
}

val answer04 = length(P04)
val answerTest04 = testAnswer(answer04 == 6)

println(f"P04: $answer04 ($answerTest04)")


//////////////////////////////////////////////////////////////////////////
// P05 
// (*) Reverse a list.
// Example:
// scala> reverse(List(1, 1, 2, 3, 5, 8))
// res0: List[Int] = List(8, 5, 3, 2, 1, 1)
val P05 = List(1, 1, 2, 3, 5, 8)

def reverse[T](list: List[T]): List[T] = {

    list match { 
        case Nil => list
        case h :: t => reverse(t) :+ h
    }
}

val answer05 = reverse(P05)
val answerTest05 = testAnswer(answer05 == List(8, 5, 3, 2, 1, 1))

println(f"P05: $answer05 ($answerTest05)")


//////////////////////////////////////////////////////////////////////////
// P06 
// (*) Find out whether a list is a palindrome.
// Example:
// scala> isPalindrome(List(1, 2, 3, 2, 1))
// res0: Boolean = true
val P06 = List(1, 2, 3, 2, 1)
val P06_2 = List(8, 5, 3, 2, 1, 1)

def isPalindrome[T](list: List[T]): Boolean = {
    // trivial with builtins
    // list == list.reverse

    // technically you can check only that the first half `
    // of the list if equal to the second half, but that's more complex'
    list.foldLeft(List[T]())((x, y) => y :: x) == list
}

val answer06 = isPalindrome(P06)
val answer06_2 = isPalindrome(P06_2)
val answerTest06 = testAnswer(answer06 && (answer06_2 == false)) 

println(f"P06: $answer06 ($answerTest06)")


//////////////////////////////////////////////////////////////////////////
// P07 
// (**) Flatten a nested list structure.
// Example:
// scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
// res0: List[Any] = List(1, 1, 2, 3, 5, 8)
val P07 = List(List(1, 1), 2, List(3, List(5, 8)))

def flatten(list: List[Any]): List[Any] = {
    list flatMap {
        case l: List[Any] => flatten(l)
        case elem => List(elem)
    }
}

val answer07 = flatten(P07)
val answerTest07 = testAnswer(answer07 == List(1, 1, 2, 3, 5, 8)) 

println(f"P07: $answer07 ($answerTest07)")


//////////////////////////////////////////////////////////////////////////
// P08 
// (**) Eliminate consecutive duplicates of list elements.
// If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
// Example:

// scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
// res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
val P08 = List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')

def compress[T](list: List[T]): List[T] = {
    list.foldRight(List[T]()) ((h, t) => if (t.isEmpty || t.head != h) h :: t else t)
}

val answer08 = compress(P08)
val answerTest08 = testAnswer(answer08 == List('a', 'b', 'c', 'a', 'd', 'e')) 

println(f"P08: $answer08 ($answerTest08)")


//////////////////////////////////////////////////////////////////////////
// P09 
// (**) Pack consecutive duplicates of list elements into sublists.
// If a list contains repeated elements they should be placed in separate sublists.
// Example:

// scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
// res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
val P09 = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

def pack[T](list: List[T]): List[List[T]] = {
    if (list.isEmpty) List(List())   // and this is trivial...
    else {
      val (packed, next) = list span { _ == list.head } // span splits a list based on a predicate in two sublists
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
}

val answer09 = pack(P09)
val answerTest09 = testAnswer(answer09 == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))) 

println(f"P09: $answer09 ($answerTest09)")


//////////////////////////////////////////////////////////////////////////
// P10 
// (*) Run-length encoding of a list.
// Use the result of problem P09 to implement the so-called run-length encoding data compression method. 
// Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
// Example:

// scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
// res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
val P10 = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

def encode[T](list: List[T]): List[Tuple2[Int, T]] = {
    pack(list) map {
        ls => (ls.length, ls.head)   // not guarding against empty lists because we know they can't be (as based on P09)'
    }
}

val answer10 = encode(P10)
val answerTest10 = testAnswer(answer10 == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))) 

println(f"P10: $answer10 ($answerTest10)")


//////////////////////////////////////////////////////////////////////////
// P11 
// (*) Modified run-length encoding.
// Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list.
// Only elements with duplicates are transferred as (N, E) terms.
// Example:

// scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
// res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
val P11 = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

def encodeModified[T](list: List[T]): List[Any] = {
    pack(list) map {
        case h :: Nil => h  
        case ls => (ls.length, ls.head)
    }
}

val answer11 = encodeModified(P11)
val answerTest11 = testAnswer(answer11 == List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))) 

println(f"P11: $answer11 ($answerTest11)")


//////////////////////////////////////////////////////////////////////////
// P12 (**)
// Decode a run-length encoded list.
// Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
// Example:

// scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
// res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
val P12 = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))

def decode[T](list: List[(Int, T)]): List[T] = {
    list flatMap {el: (Int, T) => List.fill(el._1)(el._2)}  // an alternative is using e => List.make(e._1, e._2) in flatMap  [doesn't work']
}

val answer12 = decode(P12)
val answerTest12 = testAnswer(answer12 == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) 

println(f"P12: $answer12 ($answerTest12)")


//////////////////////////////////////////////////////////////////////////
// P13 
// (**) Run-length encoding of a list (direct solution).
// Implement the so-called run-length encoding data compression method directly. 
// I.e. don't use other methods you've written (like P09's pack); do all the work directly.
// Example:

// scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
// res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
val P13 = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

def encodeDirect[T](list: List[T]): List[(Int, T)] = {
    if (list.isEmpty) Nil
    else {
      val (packed, next) = list span { _ == list.head }
      (packed.length, packed.head) :: encodeDirect(next)
    }
} 

val answer13 = encodeDirect(P13)
val answerTest13 = testAnswer(answer13 == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))) 

println(f"P13: $answer13 ($answerTest13)")


//////////////////////////////////////////////////////////////////////////
// P14 
// (*) Duplicate the elements of a list.
// Example:
// scala> duplicate(List('a, 'b, 'c, 'c, 'd))
// res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
val P14 = List('a, 'b, 'c, 'c, 'd)

def duplicate[T](list: List[T]): List[T] = {
    list flatMap {x => List(x, x) }
}

val answer14 = duplicate(P14)
val answerTest14 = testAnswer(answer14 == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))

println(f"P14: $answer14 ($answerTest14)")


//////////////////////////////////////////////////////////////////////////
// P15 
// (**) Duplicate the elements of a list a given number of times.
// Example:
// scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
// res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
// List.make(e._1, e._2)
val P15 = List('a, 'b, 'c, 'c, 'd)

def duplicateN[T](n: Int, list: List[T]): List[T] = {
    list flatMap {x => List.fill(n)(x) }
}

val answer15 = duplicateN(3, P15)
val answerTest15 = testAnswer(answer15 == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
println(f"P15: $answer15 ($answerTest15)")


//////////////////////////////////////////////////////////////////////////
// P16 
// (**) Drop every Nth element from a list.
// Example:
// scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
// res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
val P16 = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

def drop[T](n: Int, list: List[T]): List[T] = {
    list.zipWithIndex.filter( it => (it._2 + 1) % n != 0) map (_._1)
}

val answer16 = drop(3, P16)
val answerTest16 = testAnswer(answer16 == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
println(f"P16: $answer16 ($answerTest16)")


//////////////////////////////////////////////////////////////////////////
// P17 
// (*) Split a list into two parts.
// The length of the first part is given. Use a Tuple for your result.
// Example:

// scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
// res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
val P17 = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

def split[T](n: Int, list: List[T]): (List[T], List[T]) = {

    // trivial with builtins:
    // list.splitAt(n)

    //recursive
    (n, list) match {
        case (_, Nil) => (Nil, Nil)
        case (0, ls) => (Nil, ls)
        case (n, h :: t) => {
            val (start, end) = split(n - 1, t)
            (h :: start, end)
        }
    }
}

val answer17 = split(3, P17)
val answerTest17 = testAnswer(answer17 == (List('a, 'b, 'c), List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
println(f"P17: $answer17 ($answerTest17)")


//////////////////////////////////////////////////////////////////////////
// P18
// (**) Extract a slice from a list.
// Given two indices, I and K, the slice is the list containing the elements 
// from and including the Ith element up to but not including the Kth element of the original list.
// Start counting the elements with 0.
// Example:

// scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
// res0: List[Symbol] = List('d, 'e, 'f, 'g)
val P18 = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

def slice[T](low: Int, high: Int, list: List[T]): List[T] = {
    list.zipWithIndex.filter( el => el._2 >= low && el._2 < high).map {_._1}
}

val answer18 = slice(3, 7, P18)
val answerTest18 = testAnswer(answer18 == List('d, 'e, 'f, 'g))
println(f"P18: $answer18 ($answerTest18)")


//////////////////////////////////////////////////////////////////////////
// P19 
// (**) Rotate a list N places to the left.
// Examples:
// scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
// res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)

// scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
// res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
val P19 = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)

def rotate[T](n: Int, list: List[T]): List[T] = {
    val nBounded = if (list.isEmpty) 0 else n % list.length
    if (nBounded < 0) rotate(nBounded + list.length, list)
    else (list drop nBounded) ::: (list take nBounded)
}

val answer19 = rotate(3, P19)
val answer19_2 = rotate(-2, P19)
val answerTest19 = testAnswer(
    answer19 == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c) &&
    answer19_2 == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
    )
println(f"P19: $answer19 + $answer19_2 ($answerTest19)")


//////////////////////////////////////////////////////////////////////////
// P20
// (*) Remove the Kth element from a list.
// Return the list and the removed element in a Tuple. Elements are numbered from 0.
// Example:

// scala> removeAt(1, List('a, 'b, 'c, 'd))
// res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
val P20 = List('a, 'b, 'c, 'd)

def removeAt[T](n: Int, list: List[T]): (List[T], T) = {
    // val symb = List(n)
    return (list.zipWithIndex.filter { case  (x: T, y: Int) => y != n }.map{ case (x: T, y: Int) => x }, list(n))
}

val answer20 = removeAt(1, P20)
val answerTest20 = testAnswer(answer20 == (List('a, 'c, 'd),'b))
println(f"P20: $answer20 ($answerTest20)")
