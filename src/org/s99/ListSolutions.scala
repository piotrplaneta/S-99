package org.s99

object ListSolutions {
  def last[A](l: List[A]): Option[A] = l match {
    case Nil => None
    case x :: Nil => Some(x)
    case x :: xs => last(xs)
  }

  def lastButOne[A](l: List[A]): Option[A] = l match {
    case x :: _ :: Nil => Some(x)
    case x :: xs => lastButOne(xs)
    case _ => None
  }

  def nth[A](l: List[A], n: Int): Option[A] = l match {
    case x :: _ if n == 0 => Some(x)
    case x :: xs if n > 0 => nth(xs, n - 1)
    case _ => None
  }

  def length[A](l: List[A]): Int =
    l.foldRight(0)((_, acc) => acc + 1)

  def reverse[A](l: List[A]): List[A] =
    l.foldLeft(List.empty[A])((acc, x) => x :: acc)

  def isPalindrome[A](l: List[A]): Boolean =
    l.reverse == l

  def flatten(l: List[Any]): List[Any] = {
    l.foldRight(List.empty[Any])((el, acc) => el match {
      case x :: xs => x :: flatten(xs) ++ acc
      case x => x :: acc
    })
  }

  def flatMapFlatten(l: List[Any]): List[Any] = {
    l.flatMap((el) => el match {
      case xs: List[_] => flatten(xs)
      case x => List(x)
    })
  }
}