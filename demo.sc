// * @author weaves

// Introductory examples.

// * Types

// ** Wildcard Type

def f0(m:Map[_ <: AnyRef,_ <: AnyRef]):Int = {
  println(m.size)
  m.size
}

val m = sys.env

f0(m)

// *** Note

// The <: is any sub-class of AnyRef.

// ** Maps and iterations

// Demonstrates string interpolation
m.take(4).foreach( x => println(s"${x._1} -> ${x._2}"))

m.take(4).foreach( x => printf("%s -> %s\n", x._1, x._2))

for ((k,v) <- m) printf("key: %s, value: %s\n", k, v)

m foreach { case (key, value) => println (key + "-->" + value) }

// *** Note

// The last operation is the case-class idiom. This was added to because foreach()
// (and others like map) would find the statement ambiguous.

// ** Rich operations
// Some useful operations are available on the Rich Types.

0 max 5

0 min 5

// *** Post-fix Operators

// Some operators are available as post-fix. But this need to be enabled with the
// following import.

import scala.language.postfixOps

-2.7 abs

-2.7 round

1.5 isInfinity

(1.0 / 0) isInfinity

4 to 6

"bob" capitalize

"robert" drop 2

// *** Rich types

/*
 
scala.runtime.RichByte                Byte     
scala.runtime.RichShort		      Short  
scala.runtime.RichInt		      Int    
scala.runtime.RichChar		      Char   
scala.runtime.RichFloat		      Float  
scala.runtime.RichDouble	      Double 
scala.runtime.RichBoolean	      Boolean
scala.collection.immutable.StringOps  String 

http://www.scala-lang.org/api/2.11.8/index.html#scala.collection.immutable.StringOps

*/ 

// *** Type Variance

/* 

Name 	
Description 	
Scala Syntax

Invariant
C[T'] and C[T] are not related
C[T]

Covariant
C[T'] is a subclass of C[T]
C[+T]

Contravariant
C[T] is a subclass of C[T']
C[-T]

Most immutable collections are covariant, and most mutable collections are invariant

This is one of the reasons, it better to use immutable collections where possible.

*/

// In this example, the derived types can be appended to a new collection of the base type.


class Fruit

case class Apple() extends Fruit

case class Orange() extends Fruit

val l1: List[Apple] = Apple() :: Nil

val l2: List[Fruit] = Orange() :: l1

l1.head
l2.head

// **** Note

// The generic lists are covariant because of Fruit.

// *** Generic Lists

// And also, it's safe to prepend with "anything",
// as we're building a new list - not modifying the previous instance

val l3: List[AnyRef] = "" :: l2

// Array is mutable so this won't compile, either though the target is "val"
val a: Array[Any] = Array[Int](1, 2, 3)

// But a wild card will allow it.
val a: Array[_ <: Any] = Array[Int](1, 2, 3)

// To access an array element
a(0)

// ** Identifiers

// Identifiers paired back-ticks can make any token - even reserved words -
// identifiers.

val `yield`:Int = 1
println(`yield`)

val `no yield`:Int = 1
println(`no yield`)

// If it doesn't need backticks, but they're used
val `x0`:Int = 1
println(x0)

// *** Note

// Spaces and other glyphs can be used.

// ** Implicit conversions

// Introduce the Rational class. This has many features.
// In particular, there is linear construction. The require(), then the gcd(). 

// this() is an alternative constructor.

// various operator overloads.

class Rational(n: Int, d: Int) {
  require(d != 0)

  private val g = gcd(n.abs, d.abs)

  val numer = n / g
  val denom = d / g

  def this(n: Int) = this(n, 1)

  def + (that: Rational): Rational =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def + (i: Int): Rational =
    new Rational(numer + i * denom, denom)

  def - (that: Rational): Rational =
    new Rational(
      numer * that.denom - that.numer * denom,
      denom * that.denom
    )

  def - (i: Int): Rational =
    new Rational(numer - i * denom, denom)

  def * (that: Rational): Rational =
    new Rational(numer * that.numer, denom * that.denom)

  def * (i: Int): Rational =
    new Rational(numer * i, denom)

  def / (that: Rational): Rational =
    new Rational(numer * that.denom, denom * that.numer)

  def / (i: Int): Rational =
    new Rational(numer, denom * i)

  // Note that override has to be stated.
  override def toString = numer +"/"+ denom

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)
}

val r = new Rational(66, 47)

r * 2

// but not

2 * r

/// *** Add an implicit method

// so introduce an implicit using this package to notify the interpreter.

import scala.language.implicitConversions

implicit def intToRational(x: Int) = new Rational(x)

2 * r

/// **** Note: now works.

// ** Control structures and iteration

// *** listFiles is a Java method.

val filesHere = (new java.io.File(".")).listFiles

for (file <- filesHere)
  println(file)

// **** Note: java.io.File.listfiles

// This can be called without (). It returns an array.

// *** Using yield

// yield is not similar to Python's generator. It re-scopes the file variable and
// takes it as a return value.

def scalaFiles =
  for {
    file <- filesHere
    if file.getName.endsWith(".sc")
  } yield file

// *** try-catch

// Render an Array as a List
// And typing a var for a block.

import java.io.FileReader
import java.io.FileNotFoundException
import java.io.IOException

val names = scalaFiles.toList.head :: List(new java.io.File("input.txt"))

var f:java.io.FileReader = null

for (fn <- names) 
  try {
    println("file: " + fn)
    f = new FileReader(fn)
    // Use and close file
  } catch {
    case ex: FileNotFoundException => println("no file: " + fn.toString)// Handle missing file
    case ex: IOException => println("io" + fn.toString) // IO error
  } finally {
    f.close
  }

// **** Note

// Other run-time environments support Automatic Resource Management methods.

// Better solutions are .Net using () {}
// https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/using-statement

// Scala has some support, but not in the language yet?
// Scala Automatic Resource Management
// https://github.com/jsuereth/scala-arm

// *** Implementing AutoCloseable

import java.lang.AutoCloseable
import java.nio.file.{Files, Paths}
import java.util.Optional

def autoClose[A <: AutoCloseable,B](closeable: A)(fun: (A) ⇒ B): B = {
    try {
        fun(closeable)
    } finally {
        closeable.close()
    }
}

val result: Optional[String] = autoClose(Files.lines(Paths.get(fn.toString))) { stream ⇒
    stream.findAny()
}

// **** Note

// There is a trick to declaring variables for try-catch

// *** Using Option with try-catch

import java.io._

object CopyBytes extends App {
    var in = None: Option[FileInputStream]
    var out = None: Option[FileOutputStream]
    try {
        in = Some(new FileInputStream("/tmp/Test.class"))
        out = Some(new FileOutputStream("/tmp/Test.class.copy"))
        var c = 0
        while ({c = in.get.read; c != −1}) {
            out.get.write(c)
        }
    } catch {
        case e: IOException => e.printStackTrace
    } finally {
        println("entered finally ...")
        if (in.isDefined) in.get.close
        if (out.isDefined) out.get.close
    }
}

// **** Note

// You use Option, Some and None. There is an alternative - using Right and Left.
// See valuetypes.sc 

// ** Companion Objects

// Note: List is abstract so this cannot use the "new" keyword.
// List has a companion object that acts as a factory.

// Like this one

class Person {
  var name: String = _
  var age: Int = 0
}

object Person {
    def apply(name: String): Person = {
        var p = new Person
        p.name = name
        p
    }
    def apply(name: String, age: Int): Person = {
        var p = new Person
        p.name = name
        p.age = age
        p
    }
}

// *** Note: overloads of apply

// you can have overload the apply method

// *** Examples

val pete = Person("Pete")
val peter = Person("Peter", 42)

case class Person1 (var name: String)

val dave = Person1("Dave")

pete.name
peter.name
peter.age

dave.name

// **** Note

// You can see the alternative constructions.

// ** Annotations (or Decorations) 
// Scala supports these

// http://www.scala-lang.org/api/2.12.0/scala/annotation/Annotation.html

// *** Important
// tailrec 
// implicitNotFound implicitAmbiguous

// ** Switch statement
// This is replaced by match

val args = List[Any]("salt", 1)

val firstArg = if (args.length > 0) args(0) else ""
firstArg match {
  case "salt" => println("pepper")
  case "chips" => println("salsa")
  case "eggs" => println("bacon")
  case _ => println("huh?")
}

for(arg <- args) {
  arg match {
    case "salt" => println("pepper")
    case "chips" => println("salsa")
    case "eggs" => println("bacon")
    case 1 => println("1:Int")
    case _ => println("huh?")
  }
}

// *** Note
// This is very flexible, it can support mixed types too.

// ** Loop control: break and continue

// Scala does not directly support break and continue, but there is a breakable block extension.

import scala.util.control.Breaks._
import java.io._
val in = new BufferedReader(new InputStreamReader(System.in))
breakable {
  while (true) {
    println("? ")
    if (in.readLine() == "") break
  }
}

// *** Note
// This is a feature of Scala that isn't too easy to appreciate. Scala only has
// expressions. 

// ** Variable scope

// Almost like Java, but does allow new variable with a name from an outer scope.

// ** Functions and closures

// A function is a Lambda and is an object.

var increase = (x: Int) => x + 9999

// *** Some currying 

def sum(a: Int, b: Int, c: Int) = a + b + c

val sum1 = sum(1, _:Int, 3)

6 == sum1(2)

// *** Partially applied function
// Note the trailing underscore

val sum2 = sum _

// *** Closure

// By contrast, Java’s inner classes do not allow you to access
// modifiable variables in surrounding scopes at all, so there is no
// difference between capturing a variable and capturing its currently
// held value.

// This captures the value of 'more' and uses it within a lambda function.

def makeIncreaser(more: Int) = (x: Int) => x + more

val add10 = makeIncreaser(10)

val add15 = makeIncreaser(15)

add10(5)
add10(15)

add15(5)

// ** Function Parameters Definition 

// *** Repeated parameters use the star *

def echo(args: String*) = for (arg <- args) println(arg)

echo("a", "b")

// *** Arrays can be expanded when passed

val arr = Array("a", "b", "c")

echo(arr: _*)

// *** Named arguments

def speed(distance: Float, time: Float): Float =
  distance / time

/// *** Note: and by name

speed(time = 10, distance = 120)

// *** Default value for a parameter

def printTime(out: java.io.PrintStream = System.out, divisor: Int = 1) =
  out.println("time = "+ System.currentTimeMillis()/divisor)

// *** Empty brackets required
// The following doesn't work, (it returns the partially applied function)
// The brackets are required.

printTime

printTime()

printTime(out = System.err, divisor = 1000000)

// ** Tail Recursion

// It is possible to implement many methods without recursion because it may appear
// to be less expensive - less stack frames. But with tail-recursion, the Scala
// compiler can apply an optimization and use only one stack frame. (It does the
// iteration for you. Type 0 (ForTran) and Type 1 (C-based runtime) languages.)

// But the tail-recursion must be well-formed - the recursion must be the last
// invocation in the method.

// *** Tracing: bad: many stack frames

// This isn't tail-recursive because of the + 1 at the end. 

def boom(x: Int): Int = { 
  if (x == 0) throw new Exception("boom!") 
  else boom(x - 1) + 1
}

boom(3)

// **** Note
// The exception shows 4 stack frames, one for the initial invocation and then 3 for
// the recursive ones - counting down to zero.

// *** Tracing: good: one stack frame

def bang(x: Int): Int = {
  if (x == 0) throw new Exception("bang!")
  else bang(x - 1)
}

bang(3)

// The exception now shows 1 stack frame

// *** Tracing: good: one stack frame with annotation

// The annotation helps the compiler apply the stack frame optimization.

@annotation.tailrec def bang(x: Int): Int = {
  if (x == 0) throw new Exception("bang!")
  else bang(x - 1)
}

bang(3)

// *** Tracing: bad: annotation throws compile-error

@annotation.tailrec def boom(x: Int): Int = { 
  if (x == 0) throw new Exception("boom!") 
  else boom(x - 1)
}

boom(3)



// * Postamble

// The following are the file variables.

// Local Variables:
// mode:scala
// scala-edit-mark-re: "^// [\\*]+ "
// comment-column:50 
// comment-start: "// "  
// comment-end: "" 
// eval: (outline-minor-mode)
// outline-regexp: "// [*]+"
// eval: (auto-fill-mode)
// fill-column: 85 
// End: 
