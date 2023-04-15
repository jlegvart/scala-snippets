import scala.annotation.tailrec

abstract class Stream[+A] {

  def isEmpty: Boolean
  def head: A
  def tail: Stream[A]

  def #::[B >: A](element: B): Stream[B]
  def ++[B >: A](anotherStream: => Stream[B]): Stream[B]

  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): Stream[B]
  def flatMap[B](f: A => Stream[B]): Stream[B]
  def filter(predicate: A => Boolean): Stream[A]

  def take(n: Int): Stream[A]
  def takeAsList(n: Int): List[A] = take(n).toList()

  @tailrec
  final def toList[B >: A](acc: List[B] = Nil): List[B] =
    if (isEmpty)
      acc.reverse
    else
      tail.toList(head :: acc)

}

object Stream {

  def from[A](start: A)(generator: A => A): Stream[A] = LazyStream(
    start,
    Stream.from(generator(start))(generator),
  )

}

class EmptyStream extends Stream[Nothing] {

  override def flatMap[B](f: Nothing => Stream[B]): Stream[B] = this

  override def filter(predicate: Nothing => Boolean): Stream[Nothing] = this

  override def tail: Stream[Nothing] = this

  override def ++[B >: Nothing](anotherStream: => Stream[B]): Stream[B] = anotherStream

  override def take(n: Int): Stream[Nothing] = this

  override def #::[B >: Nothing](element: B): Stream[B] = LazyStream(element, this)

  override def map[B](f: Nothing => B): Stream[B] = this

  override def foreach(f: Nothing => Unit): Unit = ()

  override def head: Nothing = throw new NoSuchElementException()

  override def isEmpty: Boolean = true

}

class LazyStream[+A](hd: A, tl: => Stream[A]) extends Stream[A] {

  override def head: A = hd

  override lazy val tail: Stream[A] = tl

  override def flatMap[B](f: A => Stream[B]): Stream[B] = f(head) ++ tail.flatMap(f)

  override def filter(predicate: A => Boolean): Stream[A] =
    if (predicate(head))
      new LazyStream(head, tail.filter(predicate))
    else
      tail.filter(predicate)

  override def ++[B >: A](anotherStream: => Stream[B]): Stream[B] =
    new LazyStream(head, tail ++ anotherStream)

  override def take(n: Int): Stream[A] =
    if (n <= 0)
      new EmptyStream
    else if (n == 1)
      new LazyStream(head, new EmptyStream)
    else
      new LazyStream(head, tail.take(n - 1))

  override def #::[B >: A](element: B): Stream[B] = new LazyStream(element, this)

  override def map[B](f: A => B): Stream[B] = new LazyStream(f(head), tail.map(f))

  override def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def isEmpty: Boolean = false

}

val stream = Stream.from(1)(_ + 1)

stream.head
stream.tail.head
stream.tail.tail.head

(0 #:: stream).head

stream.map(_ * 2).take(10).toList()

stream.flatMap(i => LazyStream(i, LazyStream(i + 1, new EmptyStream))).take(10).toList()

stream.filter(_ % 2 == 0).take(10).toList()

def fib(first: Int, second: Int): Stream[Int] = new LazyStream(first, fib(second, first + second))

fib(0, 1).take(10).toList()
