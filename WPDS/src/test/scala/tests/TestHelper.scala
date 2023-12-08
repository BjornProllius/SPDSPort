package tests

import wpds.impl._
import wpds.interfaces._

object TestHelper {
    val ACC: Abstraction = a(999)

    def accepts(a: Int, c: String): PAutomaton[StackSymbol, Abstraction] = {
        val aut = new PAutomaton[StackSymbol, Abstraction] {
            override def createState(d: Abstraction, loc: StackSymbol): Abstraction = new Abstraction(d, loc)
            override def epsilon(): StackSymbol = s("EPS")
            override def isGeneratedState(d: Abstraction): Boolean = d.s != null
        }
        aut.addFinalState(ACC)
        aut.addTransition(t(a, c, ACC))
        aut
    }

    def waccepts(a: Int, c: String, weight: NumWeight): WeightedPAutomaton[StackSymbol, Abstraction, NumWeight] = {
        val aut = new WeightedPAutomaton[StackSymbol, Abstraction, NumWeight] {
            override def createState(d: Abstraction, loc: StackSymbol): Abstraction = new Abstraction(d, loc)
            override def epsilon(): StackSymbol = s("EPS")
            override def getOne(): NumWeight = NumWeight.one()
            override def isGeneratedState(d: Abstraction): Boolean = d.s != null
        }
        aut.addFinalState(ACC)
        aut.addTransition(t(a, c, ACC))
        aut.addWeightForTransition(t(a, c, ACC), weight)
        aut
    }

    def a(a: Int): Abstraction = new Abstraction(a)

    def a(a: Int, b: String): Abstraction = new Abstraction(a(a), s(b))

    def s(a: String): StackSymbol = new StackSymbol(a)

    def t(a: Abstraction, c: StackSymbol, b: Abstraction): Transition[StackSymbol, Abstraction] = new Transition[StackSymbol, Abstraction](a, c, b)

    def t(a: Abstraction, c: String, b: Abstraction): Transition[StackSymbol, Abstraction] = new Transition[StackSymbol, Abstraction](a, s(c), b)

    def t(a: Int, c: StackSymbol, b: Abstraction): Transition[StackSymbol, Abstraction] = t(a(a), c, b)

    def t(a: Int, c: String, b: Abstraction): Transition[StackSymbol, Abstraction] = t(a, s(c), b)

    def t(a: Int, c: String, b: Int): Transition[StackSymbol, Abstraction] = t(a, c, a(b))

    def normal(a: Int, n: String, b: Int, m: String): UNormalRule[StackSymbol, Abstraction] = new UNormalRule[StackSymbol, Abstraction](a(a), s(n), a(b), s(m))

    def push(a: Int, n: String, b: Int, m: String, l: String): UPushRule[StackSymbol, Abstraction] = new UPushRule[StackSymbol, Abstraction](a(a), s(n), a(b), s(m), s(l))

    def pop(a: Int, n: String, b: Int): UPopRule[StackSymbol, Abstraction] = new UPopRule[StackSymbol, Abstraction](a(a), s(n), a(b))

    def wnormal(a: Int, n: String, b: Int, m: String, w: NumWeight): NormalRule[StackSymbol, Abstraction, NumWeight] = new NormalRule[StackSymbol, Abstraction, NumWeight](a(a), s(n), a(b), s(m), w)

    def wpush(a: Int, n: String, b: Int, m: String, l: String, w: NumWeight): PushRule[StackSymbol, Abstraction, NumWeight] = new PushRule[StackSymbol, Abstraction, NumWeight](a(a), s(n), a(b), s(m), s(l), w)

    def wpop(a: Int, n: String, b: Int, w: NumWeight): PopRule[StackSymbol, Abstraction, NumWeight] = new PopRule[StackSymbol, Abstraction, NumWeight](a(a), s(n), a(b), w)

    case class Abstraction(a: Int, s: StackSymbol = null) extends State {
        override def toString: String = if (s == null) a.toString else s"<$a,$s>"
    }

    case class StackSymbol(s: String) extends Location {
        override def toString: String = s
        override def accepts(other: Location): Boolean = this == other
    }


}