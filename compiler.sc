import scala.compiletime.ops.float
import javax.swing.text.html.HTML.Tag

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp 
case class STAR(r: Rexp) extends Rexp 

// new regular expressions
case class RANGE(s: Set[Char]) extends Rexp
case class PLUS(r: Rexp) extends Rexp
case class OPTIONAL(r: Rexp) extends Rexp
case class NTIMES(r: Rexp, n: Int) extends Rexp 
case class RECD(x: String, r: Rexp) extends Rexp


// values - you might have to extend them 
// according to which values you want to create
// for the new regular expressions
abstract class Val
case object Empty extends Val
case class Chr(c: Char) extends Val
case class Sequ(v1: Val, v2: Val) extends Val
case class Left(v: Val) extends Val
case class Right(v: Val) extends Val
case class Stars(vs: List[Val]) extends Val
case class Rec(x: String, v: Val) extends Val
case class Ntimes(v: Val, n: Int) extends Val
case class Plus(v: Val) extends Val
case class Optional(v: Val) extends Val
case class Range(s: Set[Char]) extends Val


// convenience for typing regular expressions
import scala.language.implicitConversions

def charlist2rexp(s : List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}

given Conversion[String, Rexp] = (s => charlist2rexp(s.toList))

extension (r: Rexp) {
  def ~ (s: Rexp) = SEQ(r, s)
  def % = STAR(r)
  def | (s: Rexp) = ALT(r, s)
}

extension (s: String) {
  def $ (r: Rexp) = RECD(s, r)
  def | (r: Rexp) = ALT(s, r)
  def | (r: String) = ALT(s, r)
  def % = STAR(s)
  def ~ (r: Rexp) = SEQ(s, r)
  def ~ (r: String) = SEQ(s, r)
}


// nullable (needs to be extended for new regular expressions)
def nullable(r: Rexp) : Boolean = r match {
  case ZERO => false
  case ONE => true
  case CHAR(_) => false
  case ALT(r1, r2) => nullable(r1) || nullable(r2)
  case SEQ(r1, r2) => nullable(r1) && nullable(r2)
  case STAR(_) => true
  case RANGE(s) => false
  case OPTIONAL(r) => true
  case NTIMES(r, n) => n == 0 || nullable(r)
  case PLUS(r) => nullable(r)
  case RECD(x, r) => nullable(r)
}

// der (needs to be extended for new regular expressions)
def der(c: Char, r: Rexp) : Rexp = r match {
  case ZERO => ZERO
  case ONE => ZERO
  case CHAR(d) => if (c == d) ONE else ZERO
  case ALT(r1, r2) => ALT(der(c, r1), der(c, r2))
  case SEQ(r1, r2) => 
    if (nullable(r1)) ALT(SEQ(der(c, r1), r2), der(c, r2))
    else SEQ(der(c, r1), r2)
  case STAR(r) => SEQ(der(c, r), STAR(r))
  case RANGE(s) => if (s.contains(c)) ONE else ZERO
  case PLUS(r) => SEQ(der(c, r), STAR(r))
  case OPTIONAL(r) => der(c, r)
  case NTIMES(r, n) => 
    if (n == 0) ZERO
    else SEQ(der(c, r), NTIMES(r, n-1))
  case RECD(x, r) => der(c, r)
}

// flatten (needs to work with all values) 
def flatten(v: Val) : String = v match {
  case Empty => ""
  case Chr(c) => c.toString
  case Left(v) => flatten(v)
  case Right(v) => flatten(v)
  case Sequ(v1, v2) => flatten(v1) + flatten(v2)
  case Stars(vs) => vs.map(flatten).mkString
  case Rec(_, v) => flatten(v)
  case Ntimes(v, n) => List.fill(n)(flatten(v)).mkString
  case Plus(v) => flatten(v)
  case Optional(v) => flatten(v)
  case Range(s) => s.mkString
}

// env (needs to work with all values) 
def env(v: Val) : List[(String, String)] = v match {
  case Empty => Nil
  case Chr(c) => Nil
  case Left(v) => env(v)
  case Right(v) => env(v)
  case Sequ(v1, v2) => env(v1) ::: env(v2)
  case Stars(vs) => vs.flatMap(env)
  case Rec(x, v) => (x, flatten(v))::env(v)
  case Ntimes(v, n) => env(v)
  case Plus(v) => env(v)
  case Optional(v) => env(v)
  case Range(s) => Nil
}

// mkeps (needs to be extended for new regular expressions)
def mkeps(r: Rexp) : Val = r match {
  case ONE => Empty
  case ALT(r1, r2) => 
    if (nullable(r1)) Left(mkeps(r1)) else Right(mkeps(r2))
  case SEQ(r1, r2) => Sequ(mkeps(r1), mkeps(r2))
  case STAR(r) => Stars(Nil)
  case OPTIONAL(r) => if (nullable(r)) mkeps(r)
    else Empty
  case PLUS(r) => mkeps(r)
  case NTIMES(r, n) => Stars(List.fill(n)(mkeps(r)))
  case RECD(x, r) => Rec(x, mkeps(r))
  
}

// inj (needs to be extended for new regular expressions)
def inj(r: Rexp, c: Char, v: Val) : Val = (r, v) match {
  case (STAR(r), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1)::vs)
  case (SEQ(r1, r2), Sequ(v1, v2)) => Sequ(inj(r1, c, v1), v2)
  case (SEQ(r1, r2), Left(Sequ(v1, v2))) => Sequ(inj(r1, c, v1), v2)
  case (SEQ(r1, r2), Right(v2)) => Sequ(mkeps(r1), inj(r2, c, v2))
  case (ALT(r1, r2), Left(v1)) => Left(inj(r1, c, v1))
  case (ALT(r1, r2), Right(v2)) => Right(inj(r2, c, v2))
  case (CHAR(d), Empty) => Chr(c)
  case (PLUS(r), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1)::vs)
  case (OPTIONAL(r), Empty) => inj(r, c, Empty)
  case (OPTIONAL(r), v) => inj(r, c, v) 
  case (NTIMES(r, n), Sequ(v1, Stars(vs))) => Stars(inj(r, c, v1)::vs)
  case (RANGE(s), Empty) => Chr(c)
  case (RECD(x, r1), _) => Rec(x, inj(r1, c, v))
}


// the simplification and rectification part
// can be left untouched

// rectification functions
def F_ID(v: Val): Val = v
def F_RIGHT(f: Val => Val) = (v:Val) => Right(f(v))
def F_LEFT(f: Val => Val) = (v:Val) => Left(f(v))
def F_ALT(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
  case Right(v) => Right(f2(v))
  case Left(v) => Left(f1(v))
}
def F_SEQ(f1: Val => Val, f2: Val => Val) = (v:Val) => v match {
  case Sequ(v1, v2) => Sequ(f1(v1), f2(v2))
}
def F_SEQ_Empty1(f1: Val => Val, f2: Val => Val) = 
  (v:Val) => Sequ(f1(Empty), f2(v))
def F_SEQ_Empty2(f1: Val => Val, f2: Val => Val) = 
  (v:Val) => Sequ(f1(v), f2(Empty))
def F_RECD(f: Val => Val) = (v:Val) => v match {
  case Rec(x, v) => Rec(x, f(v))
}
def F_ERROR(v: Val): Val = throw new Exception("error")

// simp
def simp(r: Rexp): (Rexp, Val => Val) = r match {
  case ALT(r1, r2) => {
    val (r1s, f1s) = simp(r1)
    val (r2s, f2s) = simp(r2)
    (r1s, r2s) match {
      case (ZERO, _) => (r2s, F_RIGHT(f2s))
      case (_, ZERO) => (r1s, F_LEFT(f1s))
      case _ => if (r1s == r2s) (r1s, F_LEFT(f1s))
                else (ALT (r1s, r2s), F_ALT(f1s, f2s)) 
    }
  }
  case SEQ(r1, r2) => {
    val (r1s, f1s) = simp(r1)
    val (r2s, f2s) = simp(r2)
    (r1s, r2s) match {
      case (ZERO, _) => (ZERO, F_ERROR)
      case (_, ZERO) => (ZERO, F_ERROR)
      case (ONE, _) => (r2s, F_SEQ_Empty1(f1s, f2s))
      case (_, ONE) => (r1s, F_SEQ_Empty2(f1s, f2s))
      case _ => (SEQ(r1s,r2s), F_SEQ(f1s, f2s))
    }
  }
  case r => (r, F_ID) // this part handles all new regular expressions
}

// lexing generating a value
def lex_simp(r: Rexp, s: List[Char]) : Val = s match {
  case Nil => if (nullable(r)) mkeps(r) else 
    { throw new Exception("lexing error") } 
  case c::cs => {
    val (r_simp, f_simp) = simp(der(c, r))
    inj(r, c, f_simp(lex_simp(r_simp, cs)))
  }
}

// lexing extracting a list of String-String pairs 
def lexing_simp(r: Rexp, s: String) : List[(String, String)] = 
  env(lex_simp(r, s.toList))


// Language specific code for the While Language 
// (you need to create the regular expressions - see CW2) 

val KEYWORD: Rexp = "if" | "then" | "else" | "def" | "val" | "print_char" | "skip" | "print_int" | "print_space" | "print_star" | "print_string"
val TYPE: Rexp = "Int" | "Double" | "Void" | "Char"
val OP: Rexp = "+" | "-" | "*" | "/" | "==" | "<=" | ">=" | "=" | ">" | "<" | "%" | ":" | "," | "!="
val LET: Rexp = RANGE("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz".toSet)
val DIGIT: Rexp = RANGE("0123456789".toSet)
val SYM: Rexp = LET | "_" | ">" | "<" | "=" | "-" | ";" | "," | ":" | "."
val PARENS: Rexp = "(" | ")" | "{" | "}"
val SEMI: Rexp = ";"
val WHITESPACE: Rexp = PLUS(" " | "\n" | "\t" | "\r")
val ID: Rexp = LET ~ (LET | "_" | DIGIT).%
val INT: Rexp = OPTIONAL("-") ~ (DIGIT | (RANGE("123456789".toSet) ~ DIGIT.%))
val FLOAT: Rexp = OPTIONAL("-") ~ (DIGIT | (RANGE("123456789".toSet) ~ DIGIT.%)) ~ "." ~ PLUS(DIGIT)
val CH: Rexp = "'" ~ (SYM | DIGIT | " " | "\\n" | "\\t" | "\\r") ~ "'"
val EOL: Rexp = "\n" | "\r\n"
val COMMENT: Rexp = "//" ~ (SYM | DIGIT | PARENS | " " | "\"").% ~ EOL
val STRING: Rexp = "\"" ~ (SYM | DIGIT | PARENS | " ").% ~ "\""

val FUNC_REGS = (
  ("c" $ COMMENT) |
  ("k" $ KEYWORD) |
  ("f" $ FLOAT) |  
  ("n" $ INT) |
  ("o" $ OP) |
  ("p" $ PARENS) |
  ("s" $ SEMI) |
  ("w" $ WHITESPACE) |
  ("str" $ STRING) |
  ("t" $ TYPE) |
  ("i" $ ID)|    
  ("h" $ CH)).%

// Token definitions
abstract class Token extends Serializable
case class T_KEYWORD(s: String) extends Token
case class T_OP(s: String) extends Token
case class T_PAREN(s: String) extends Token
case object T_SEMI extends Token
case class T_ID(s: String) extends Token
case class T_INT(n: Int) extends Token          
case class T_FLOAT(d: Double) extends Token      
case class T_CHAR(c: Int) extends Token          
case class T_COMMENT(s: String) extends Token
case class T_WHITESPACE(s: String) extends Token
case class T_TYPE(s: String) extends Token
case class T_STRING(s: String) extends Token

def processChar(s: String): Int = {
  val chars = s.substring(1, s.length - 1)
  
  chars match {
    case "\\n" => '\n'.toInt
    case "\\t" => '\t'.toInt
    case "\\r" => '\r'.toInt
    case "\\'" => '\''.toInt
    case "\\\\" => '\\'.toInt
    case c if c.length == 1 => c.charAt(0).toInt
  }
}

// Token matcher
val token: PartialFunction[(String, String), Token] = {
  case ("k", s) => T_KEYWORD(s)
  case ("o", s) => T_OP(s)
  case ("p", s) => T_PAREN(s)
  case ("s", _) => T_SEMI
  case ("str" , s) => T_STRING(s)
  case ("i", s) => T_ID(s)
  case ("n", s) => T_INT(s.toInt)
  case ("f", s) => T_FLOAT(s.toDouble)
  case ("h", s) => T_CHAR(processChar(s))
  case ("c", s) => T_COMMENT(s)
  case ("w", s) => T_WHITESPACE(s)
  case ("t", s) => T_TYPE(s)
}
// Tokenise
def tokenise(s: String) : List[Token] =
  lexing_simp(FUNC_REGS, s).collect(token).filter({
    case T_COMMENT(_) => false
    case T_WHITESPACE(_) => false
    case _ => true
  })

case class ~[+A, +B](_1: A, _2: B)
type IsSeq[I] = I => Seq[_]

abstract class Parser[I, T](using is: IsSeq[I]) {
  def parse(in: I): Set[(T, I)]

  def parse_all(in: I): Set[T] = 
    for {
      (hd, tl) <- parse(in)
      if is(tl).isEmpty
    } yield hd
}

def parse_tks(tokens: List[Token]) = Prog.parse_all(tokens)


// parser combinators

// alternative parser
class AltParser[I : IsSeq, T](p: => Parser[I, T], 
                              q: => Parser[I, T]) extends Parser[I, T] {
  def parse(in: I) = p.parse(in) ++ q.parse(in)   
}
// sequence parser
class SeqParser[I : IsSeq, T, S](p: => Parser[I, T], 
                                 q: => Parser[I, S]) extends Parser[I, ~[T, S]] {
  def parse(in: I) = 
    for ((hd1, tl1) <- p.parse(in); 
         (hd2, tl2) <- q.parse(tl1)) yield (new ~(hd1, hd2), tl2)
}


// map parser
class MapParser[I : IsSeq, T, S](p: => Parser[I, T], 
                         f: T => S) extends Parser[I, S] {
  def parse(in: I) = for ((hd, tl) <- p.parse(in)) yield (f(hd), tl)
}


// some convenient syntax for parser combinators
extension [I: IsSeq, T](p: Parser[I, T]) {
  def ||(q : => Parser[I, T]) = new AltParser[I, T](p, q)
  def ~[S] (q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
  def map[S](f: => T => S) = new MapParser[I, T, S](p, f)
}

case class TokParser(tok: Token) extends Parser[List[Token], Token] {
  def parse(ts: List[Token]) = ts match {
    case t::ts if (t == tok) => Set((t, ts)) 
    case _ => Set ()
  }
}

implicit def token2tparser(t: Token) : Parser[List[Token], Token] = TokParser(t)


extension (t: Token) {
  def || (q : => Parser[List[Token], Token]) = new AltParser[List[Token], Token](t, q)
  def map[S] (f: => Token => S) = new MapParser[List[Token], Token, S](t, f)
  def ~[S](q : => Parser[List[Token], S]) = new SeqParser[List[Token], Token, S](t, q)
}

case object IntParser extends Parser[List[Token], Int] {
  def parse(ts: List[Token]) = ts match {
    case T_INT(n)::ts => Set((n, ts)) 
    case _ => Set ()
  }
}

case object FloatParser extends Parser[List[Token], Double] {
  def parse(ts: List[Token]) = ts match {
    case T_FLOAT(n)::ts => Set((n, ts)) 
    case _ => Set ()
  }
}

case object CharParser extends Parser[List[Token], Int] {
  def parse(ts: List[Token]) = ts match {
    case T_CHAR(n)::ts => Set((n, ts)) 
    case _ => Set ()
  }
}

case object StringParser extends Parser[List[Token], String] {
  def parse(ts: List[Token]) = ts match {
    case T_STRING(s)::ts => Set((s, ts)) 
    case _ => Set ()
  }
}

case object IdParser extends Parser[List[Token], String] {
  def parse(ts: List[Token]) = ts match {
    case T_ID(s)::ts => Set((s, ts)) 
    case _ => Set ()
  }
}

val ParamParser: Parser[List[Token], (String, String)] = 
  (IdParser ~ T_OP(":") ~ TypeParser).map{ 
    case id ~ _ ~ typ => (id, typ)
  }

case object TypeParser extends Parser[List[Token], String] {
  def parse(ts: List[Token]) = ts match {
    case T_TYPE(s)::ts => Set((s, ts)) 
    case _ => Set ()
  }
}

def ListParser[I, T, S](p: => Parser[I, T], q: => Parser[I, S])(using is: I => Seq[?]): Parser[I, List[T]] = {
  (p ~ q ~ ListParser(p, q)).map{ case (x:T) ~ (y:S) ~ (z:List[T]) => x :: z } ||
  (p.map[List[T]]{s => List(s)})
}

// CW5


// Abstract Syntax Trees for the typed Fun-language
// (this can be part of the parser file, if mor convenient)

abstract class Exp
abstract class BExp
abstract class Decl

case class Def(name: String, args: List[(String, String)], ty: String, body: Exp) extends Decl
case class Main(e: Exp) extends Decl
case class Const(name: String, v: Int) extends Decl
case class FConst(name: String, x: Double) extends Decl

case class Call(name: String, args: List[Exp]) extends Exp
case class If(a: BExp, e1: Exp, e2: Exp) extends Exp
case class Var(s: String) extends Exp
case class Num(i: Int) extends Exp  // integer numbers
case class FNum(i: Double) extends Exp  // float numbers
case class ChConst(c: Int) extends Exp  // character constants
case class Aop(o: String, a1: Exp, a2: Exp) extends Exp
case class Sequence(e1: Exp, e2: Exp) extends Exp
case class Bop(o: String, a1: Exp, a2: Exp) extends BExp
case class Print_Int(e: Exp) extends Exp
case class Print_Char(e: Exp) extends Exp
case class Print_Space() extends Exp
case class Print_Star() extends Exp
case class Print_String(s: String) extends Exp
case class Skip() extends Exp


lazy val Exp: Parser[List[Token], Exp] = {
  (T_KEYWORD("if") ~ BExp ~ T_KEYWORD("then") ~ Exp ~ T_KEYWORD("else") ~ Exp).map{
    case _ ~ b ~ _ ~ e1 ~ _ ~ e2 => If(b, e1, e2): Exp
  } ||
  (T_KEYWORD("if") ~ BExp ~ T_KEYWORD("then") ~ T_PAREN("{") ~ Exp ~ T_PAREN("}") ~ T_KEYWORD("else") ~ Exp).map{
    case _ ~ b ~ _ ~ _ ~ e1 ~ _ ~ _ ~ e2 => If(b, e1, e2): Exp
  } ||
  (T_KEYWORD("if") ~ BExp ~ T_KEYWORD("then") ~ T_PAREN("{") ~ Exp ~ T_PAREN("}") ~ T_KEYWORD("else") ~ T_PAREN("{") ~ Exp ~ T_PAREN("}")).map{
    case _ ~ b ~ _ ~ _ ~ e1 ~ _ ~ _ ~ _ ~ e2 ~ _  => If(b, e1, e2): Exp
  } ||
  (T_KEYWORD("if") ~ BExp ~ T_KEYWORD("then") ~ Exp ~ T_KEYWORD("else") ~ T_PAREN("{") ~ Exp ~ T_PAREN("}")).map{
    case _ ~ b ~ _ ~ e1 ~ _  ~ _ ~ e2 ~ _ => If(b, e1, e2): Exp
  } ||
  (M ~ T_SEMI ~ Exp).map{ case e1 ~ _ ~ e2 => Sequence(e1, e2): Exp } ||
  M
}

lazy val M: Parser[List[Token], Exp] = {
  (T_KEYWORD("print_int") ~ T_PAREN("(") ~ L ~ T_PAREN(")")).map{
    case _ ~ _ ~ e ~ _ => Print_Int(e): Exp
  } ||
  (T_KEYWORD("print_char") ~ T_PAREN("(") ~ L ~ T_PAREN(")")).map{
    case _ ~ _ ~ e ~ _ => Print_Char(e): Exp
  } ||
  (T_KEYWORD("print_space") ~ T_PAREN("(") ~ T_PAREN(")")).map{
    case _ ~ _ ~  _ => Print_Space(): Exp
  } ||
  (T_KEYWORD("print_star") ~ T_PAREN("(") ~ T_PAREN(")")).map{
    case _ ~ _ ~ _ => Print_Star(): Exp
  } ||
  (T_KEYWORD("print_string") ~ T_PAREN("(") ~ StringParser ~ T_PAREN(")")).map{
    case _ ~ _ ~ s ~ _ => Print_String(s): Exp
  } ||
  (T_KEYWORD("skip") ~ T_PAREN("(") ~ T_PAREN(")")).map{ case _ ~ _ ~ _ => Skip(): Exp } ||
  (T_KEYWORD("skip")).map{ case _ => Skip(): Exp } ||
  L
}

lazy val L: Parser[List[Token], Exp] = {
  (T ~ T_OP("+") ~ L).map{ case x ~ _ ~ z => Aop("+", x, z): Exp } ||
  (T ~ T_OP("-") ~ L).map{ case x ~ _ ~ z => Aop("-", x, z): Exp } ||
  T
}

lazy val T: Parser[List[Token], Exp] = {
  (F ~ T_OP("*") ~ T).map{ case x ~ _ ~ z => Aop("*", x, z): Exp } ||
  (F ~ T_OP("/") ~ T).map{ case x ~ _ ~ z => Aop("/", x, z): Exp } ||
  (F ~ T_OP("%") ~ T).map{ case x ~ _ ~ z => Aop("%", x, z): Exp } ||
  F
}

lazy val F: Parser[List[Token], Exp] = {
  (IdParser ~ T_PAREN("(") ~ (ListParser(Exp, T_OP(","))) ~ T_PAREN(")")).map{
    case name ~ _ ~ args ~ _ => Call(name, args): Exp
  } ||
  (IdParser ~ T_PAREN("(") ~ T_PAREN(")")).map{
    case name ~ _  ~ _ => Call(name, List()): Exp
  } ||
  (T_PAREN("(") ~ Exp ~ T_PAREN(")")).map{ case _ ~ e ~ _ => e } ||
  IdParser.map(x => Var(x): Exp) ||
  IntParser.map(x => Num(x): Exp) ||
  FloatParser.map(x => FNum(x): Exp) ||
  CharParser.map(x => ChConst(x): Exp)
}

// Boolean expressions
lazy val BExp: Parser[List[Token], BExp] = {
  (Exp ~ T_OP("==") ~ Exp).map{ case x ~ _ ~ z => Bop("==", x, z): BExp } ||
  (Exp ~ T_OP("!=") ~ Exp).map{ case x ~ _ ~ z => Bop("!=", x, z): BExp } ||
  (Exp ~ T_OP("<") ~ Exp).map{ case x ~ _ ~ z => Bop("<", x, z): BExp } ||
  (Exp ~ T_OP(">") ~ Exp).map{ case x ~ _ ~ z => Bop(">", z, x): BExp } ||
  (Exp ~ T_OP("<=") ~ Exp).map{ case x ~ _ ~ z => Bop("<=", x, z): BExp } ||
  (Exp ~ T_OP(">=") ~ Exp).map{ case x ~ _ ~ z => Bop(">=", z, x): BExp }
}

// Declarations
lazy val Decl: Parser[List[Token], Decl] = {
  lazy val Defi: Parser[List[Token], Decl] = {
      (T_KEYWORD("def") ~ IdParser ~ T_PAREN("(") ~ (ListParser(ParamParser, T_OP(","))) ~ T_PAREN(")") ~ 
       T_OP(":") ~ TypeParser ~ T_OP("=") ~ T_PAREN("{") ~ Exp ~ T_PAREN("}")).map{
        case _ ~ name ~ _ ~ args ~ _ ~ _ ~ typ ~ _ ~ _ ~ body ~ _ =>
          Def(name, args, typ, body): Decl
       } ||
      (T_KEYWORD("def") ~ IdParser ~ T_PAREN("(") ~ T_PAREN(")") ~ 
       T_OP(":") ~ TypeParser ~ T_OP("=") ~ T_PAREN("{") ~ Exp ~ T_PAREN("}")).map{
        case _ ~ name ~ _ ~  _ ~ _ ~ typ ~ _ ~ _ ~ body ~ _ =>
          Def(name, List(), typ, body): Decl
      }
      ||
      (T_KEYWORD("def") ~ IdParser ~ T_PAREN("(") ~ (ListParser(ParamParser, T_OP(","))) ~ T_PAREN(")") ~ 
       T_OP(":") ~ TypeParser ~ T_OP("=") ~ Exp).map{
        case _ ~ name ~ _ ~ args ~ _ ~ _ ~ typ ~ _ ~ body =>
          Def(name, args, typ, body): Decl
       } ||
      (T_KEYWORD("def") ~ IdParser ~ T_PAREN("(") ~ T_PAREN(")") ~ 
       T_OP(":") ~ TypeParser ~ T_OP("=") ~ Exp).map{
        case _ ~ name ~ _ ~ _ ~ _ ~ typ ~ _ ~ body =>
          Def(name, List(), typ, body): Decl
       }
    }

  // Constants
  val intConst = (T_KEYWORD("val") ~ IdParser ~ T_OP(":") ~ TypeParser ~ T_OP("=") ~ IntParser).map{
    case _ ~ name ~ _ ~ _ ~ _ ~ value => Const(name, value): Decl
  }

  val floatConst = (T_KEYWORD("val") ~ IdParser ~ T_OP(":") ~ TypeParser ~ T_OP("=") ~ FloatParser).map{
    case _ ~ name ~ _ ~ _ ~ _ ~ value => FConst(name, value): Decl
  }

  intConst || floatConst || Defi
}

// Program
lazy val Prog: Parser[List[Token], List[Decl]] = {
  def RestProgram: Parser[List[Token], List[Decl]] = 
    (Decl ~ T_SEMI ~ RestProgram).map{ case d ~ _ ~ rest => d :: rest } ||
    (Decl ~ T_SEMI).map{ case d ~ _ => List(d) } ||
    (Exp).map(e => List(Main(e))) ||
    (T_PAREN("{") ~ Exp ~ T_PAREN("}")).map{ case _ ~ e ~ _ => List(Main(e)) }
  
  RestProgram
}



abstract class KVal
abstract class KExp
abstract class KDecl

case class KConst(name: String, v: Int) extends KDecl
case class KFConst(name: String, x: Double) extends KDecl
case class KIf(x1: String, e1: KExp, e2: KExp) extends KExp {
  def pad(e: KExp) = e.toString.replaceAll("(?m)^", "  ")

  override def toString = 
     s"IF $x1\nTHEN\n${pad(e1)}\nELSE\n${pad(e2)}"
}
case class KVar(s: String, ty: Ty) extends KVal
case class KNum(i: Int) extends KVal
case class KFNum(i: Double) extends KVal
case class KChConst(c: Int) extends KVal
case class Kop(o: String, v1: KVal, v2: KVal) extends KVal
case class KPrint_Int(e: KVal) extends KVal
case class KPrint_Char(e: KVal) extends KVal 
case class KPrint_Space() extends KVal
case class KPrint_Star() extends KVal
case class KPrint_String(s: String) extends KVal
case class Kvoid() extends KVal
case class KSkip() extends KVal
case class KLet(x: String, e1: KVal, e2: KExp) extends KExp {
  override def toString = s"LET $x = $e1 in \n$e2" 
}
case class KReturn(v: KVal) extends KExp
case class KCall (name: String, args: List[KVal]) extends KVal
case class KLoad(ty: String, name: String) extends KVal


// for generating new labels
var counter = -1

def Fresh(x: String) = {
  counter += 1
  x ++ "_" ++ counter.toString()
}

// typing
type Ty = String
type TyEnv = Map[String, Ty]
type ConstEnv = Map[String, String]

// initial typing environment
val initialEnv = Map[String, Ty]("skip" -> "Void", "print_int" -> "Void", "print_char" -> "Void",
                                 "print_space" -> "Void", "print_star" -> "Void", "new_line" -> "Void")

val typeConversion = Map("Int" -> "i32", "Double" -> "double", "Void" -> "void")
type ArgEnv = Map[String, List[(String, String)]]

def buildEnvironment(prog: List[Decl]): TyEnv = {
  def getTypeFromDecl(d: Decl): (String, Ty) = d match {
    case Def(name, args, retType, _) => (name, retType)
    case _ => ("", "Void") 
  }
  val functionTypes = prog.map(getTypeFromDecl).filter(_._1 != "")
  initialEnv ++ functionTypes.toMap
}
def buildArgEnv(prog: List[Decl]): ArgEnv = {
  def getArgsFromDecl(d: Decl): (String, List[(String, String)]) = d match {
    case Def(name, args, _, _) => (name, args)
    case _ => ("", List()) 
  }
  val functionArgs = prog.map(getArgsFromDecl).filter(_._1 != "")
  functionArgs.toMap
}

def buildConstEnv(prog: List[Decl]): ConstEnv = {
  def getConstsFromDecl(d: Decl): Option[(String, String)] = d match {
    case Const(name, _) => Some(name -> "i32")
    case FConst(name, _) => Some(name -> "double")
    case _ => None
  }
  prog.flatMap(getConstsFromDecl).toMap
}


// CPS translation from Exps to KExps using a
// continuation k.
def inferType(value: KVal, env: TyEnv): Ty = value match {
  case KNum(_) => "int"
  case KFNum(_) => "double"
  case KVar(_, ty) => ty
  case Kop(_, x1, x2) =>
    (inferType(x1, env), inferType(x2, env)) match {
      case ("double", _) | (_, "double") => "double"
      case _ => "int"
    }
  case KChConst(_) => "char"
  case _ => throw new Exception(s"Cannot infer type for $value")
}

def CPS(e: Exp, env: TyEnv, con: ConstEnv)(k: KVal => KExp): KExp = e match {
  case Var(s) =>
    con.get(s) match {
      case Some((ty)) =>
        val tmpVar = Fresh("tmp")
        KLet(tmpVar, KLoad(ty, s), k(KVar(tmpVar, ty)))
      case None =>
        val ty = env.getOrElse(s, "UNDEF")
        k(KVar(s, ty))
    }
  case Num(i) => k(KNum(i))
  case FNum(i) => k(KFNum(i))
  case ChConst(c) => k(KChConst(c))
  case Aop(o, e1, e2) =>
    val z = Fresh("tmp")
    CPS(e1, env, con)(y1 =>
      CPS(e2, env, con)(y2 => KLet(z, Kop(o, y1, y2), k(KVar(z, inferType(Kop(o, y1, y2), env))))))
  case If(Bop(o, b1, b2), e1, e2) =>
    val z = Fresh("tmp")
    CPS(b1, env, con)(y1 =>
      CPS(b2, env, con)(y2 =>
        KLet(z, Kop(o, y1, y2), KIf(z, CPS(e1, env,con)(k), CPS(e2, env,con)(k)))))
  case Call(name, args) =>
    def aux(args: List[Exp], vs: List[KVal]): KExp = args match {
      case Nil =>
        val ty = env.getOrElse(name, "UNDEF")
        if ty == "Void" then
          KLet("_", KCall(name, vs), k(KVar("_", "Void")))
        else {
        val z = Fresh("tmp")
        KLet(z, KCall(name, vs), k(KVar(z, env(name))))
        }
      case e :: es => CPS(e, env, con)(y => aux(es, vs ::: List(y)))
        
    }
    aux(args, Nil)
  case Sequence(e1, e2) =>
    CPS(e1, env, con)(_ => CPS(e2, env, con)(y2 => k(y2)))
  case Print_Int(e) =>
    CPS(e, env, con)(y => KLet("_", KPrint_Int(y), k(KVar("_", "Void"))))
  case Print_Char(e) =>
    CPS(e, env, con)(y => KLet("_", KPrint_Char(y), k(KVar("_", "Void"))))
  case Print_String(s) =>
    KLet("_", KPrint_String(s), k(KVar("_", "Void")))
  case Print_Space() =>
    KLet("_", KPrint_Space(), k(KVar("_", "Void")))
  case Print_Star() =>
    KLet("_", KPrint_Star(), k(KVar("_", "Void")))
  case Skip() =>
    KLet("_", KSkip(), k(KVar("_", "Void")))
}


//initial continuation
def CPSi(e: Exp, env: TyEnv, cons: ConstEnv) = CPS(e, env,cons)(KReturn(_))


// prelude
val prelude = """
declare i32 @printf(i8*, ...)

@.str_nl = private constant [2 x i8] c"\0A\00"
@.str_star = private constant [2 x i8] c"*\00"
@.str_space = private constant [2 x i8] c" \00"
@.str_int = private constant [3 x i8] c"%d\00"
@.str_c = private constant [3 x i8] c"%c\00"

define void @new_line() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_nl, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @print_star() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_star, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @print_space() #0 {
  %t0 = getelementptr [2 x i8], [2 x i8]* @.str_space, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %t0)
  ret void
}

define void @print_int(i32 %x) {
  %t0 = getelementptr [3 x i8], [3 x i8]* @.str_int, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %t0, i32 %x) 
  ret void
}

define void @print_char(i32 %x) {
  %t0 = getelementptr [3 x i8], [3 x i8]* @.str_c, i32 0, i32 0
  call i32 (i8*, ...) @printf(i8* %t0, i32 %x)
  ret void
}

define void @skip() #0 {
  ret void
}

; END OF BUILT-IN FUNCTIONS (prelude)
"""

import scala.language.implicitConversions
import scala.language.reflectiveCalls

extension (sc: StringContext) {
    def i(args: Any*): String = "   " ++ sc.s(args:_*) ++ "\n"
    def l(args: Any*): String = sc.s(args:_*) ++ ":\n"
    def m(args: Any*): String = sc.s(args:_*) ++ "\n"
}

// mathematical and boolean operations
def compile_op(op: String, x1: KVal, x2: KVal, env: TyEnv) = {
  def isDouble(v: KVal): Boolean = v match {
    case KFNum(_) => true
    case KVar(_, "double") => true 
    case _ => false
  }
  
  (isDouble(x1), isDouble(x2)) match {
    case (true, true) | (true, false) | (false, true) => op match { 
      case "+" => "fadd double "
      case "*" => "fmul double "
      case "-" => "fsub double "
      case "/" => "fdiv double "
      case "==" => "fcmp oeq double "
      case "<=" => "fcmp ole double "
      case "<"  => "fcmp olt double "
      case "!=" => "fcmp one double "
    }
    case (false, false) => op match {  
      case "+" => "add i32 "
      case "*" => "mul i32 "
      case "-" => "sub i32 "
      case "/" => "sdiv i32 "
      case "%" => "srem i32 "
      case "==" => "icmp eq i32 "
      case "<=" => "icmp sle i32 "
      case "<"  => "icmp slt i32 "
      case "!=" => "icmp ne i32 "
    }
  }
}



// compile K values
def compile_val(v: KVal, env: TyEnv, argEnv: ArgEnv) : String = v match {
  case KNum(i) => s"$i"
  case KVar(s, _) => s"%$s"
  case KFNum(i) => s"$i"
  case KChConst(c) => s"$c"
  case Kop(op, x1, x2) => 
    s"${compile_op(op, x1, x2, env)} ${compile_val(x1, env, argEnv)}, ${compile_val(x2, env, argEnv)}"
  case KCall(x1, args) => {
    val ty = typeConversion(env(x1))
    if args.isEmpty then
      s"call $ty @$x1 ()"
    else
      val expectedArgs = argEnv(x1)
      val compiledArgs = args.zip(expectedArgs).map { case (arg, (_, expectedType)) =>
        s"${typeConversion(expectedType)} ${compile_val(arg, env, argEnv)}"
      }
      s"call $ty @$x1 (${compiledArgs.mkString(", ")})"
    }
  case KPrint_Int(e) => s"call void @print_int (i32 ${compile_val(e, env,argEnv)})"
  case KPrint_Char(e) => s"call void @print_char (i32 ${compile_val(e, env,argEnv)})"
  case KPrint_String(s) => {
  val charPrints = s.map { char =>
      val ascii = char.toInt
      s"call void @print_char(i32 $ascii)"
    }.mkString("\n")
    charPrints 
  }
  case KPrint_Space() => s"call void @print_space ()"
  case KPrint_Star() => s"call void @print_star ()"
  case KSkip() => s"call void @skip ()"
  case KLoad(ty, name) => s"load $ty, $ty* @$name"
}

// compile K expressions
def compile_exp(a: KExp, env : TyEnv, argEnv: ArgEnv) : String = a match {
  case KReturn(v) =>
    v match {
      case KVar("_", _) => i"ret void"
      case _ => i"ret i32 ${compile_val(v, env, argEnv)}"
    }
  case KLet(x: String, v: KVal, e: KExp) => {
  if x=="_" then
    i"${compile_val(v, env, argEnv)}" ++ compile_exp(e, env, argEnv)
  else
    i"%$x = ${compile_val(v, env,argEnv)}" ++ compile_exp(e, env,argEnv)
}
  case KIf(x, e1, e2) => {
    val if_br = Fresh("if_branch")
    val else_br = Fresh("else_branch")
    i"br i1 %$x, label %$if_br, label %$else_br" ++
    l"\n$if_br" ++
    compile_exp(e1, env,argEnv) ++
    l"\n$else_br" ++ 
    compile_exp(e2, env,argEnv)
  }
}

def compile_decl(d: Decl, env: TyEnv, argEnv: ArgEnv, constEnv: ConstEnv): String = d match {
  case Def(name, args, ty, body) => {
    val returnType = typeConversion(ty)
    
    val argEnvWithTypes = args.map {
      case (argName, argType) => 
        (argName, typeConversion(argType)) 
    }.toMap

    val updatedEnv = env ++ argEnvWithTypes
    
    val argsString = args.map {
      case (argName, argType) => 
        s"${typeConversion(argType)} %$argName"
    }.mkString(", ")
    
    m"define $returnType @$name (${argsString}) {" ++
      compile_exp(CPSi(body, updatedEnv, constEnv), updatedEnv, argEnv) ++
    m"}\n"
  }

  case Main(body) => {
    m"define i32 @main() {" ++
      compile_exp(CPS(body, env, constEnv)(_ => KReturn(KNum(0))), env, argEnv) ++
    m"}\n"
  }

  case Const(name, v) => {
    m"@$name = global i32 $v"
  }

  case FConst(name, x) => {
    m"@$name = global double $x"
  }
}



// main compilation function

def fun_compile(prog: List[Decl]) : String =
  val constEnv = buildConstEnv(prog)
  val env = buildEnvironment(prog)
  val argEnv = buildArgEnv(prog)
  prelude ++ prog.map(decl => compile_decl(decl, env, argEnv, constEnv)).mkString



// write ll-file into a file

@main
def write(fname: String) = {
    val path = os.pwd / fname
    val file = fname.stripSuffix("." ++ path.ext)
    val tks = tokenise(os.read(path))
    val ast = parse_tks(tks).head
    val code = fun_compile(ast)
    os.write.over(os.pwd / (file ++ ".ll"), code)
}
