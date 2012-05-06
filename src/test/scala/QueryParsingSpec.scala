
import com.github.igor_petruk.furrymap.query.{FBoolean, IterableSelector}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.github.igor_petruk.furrymap.persistence._
import com.github.igor_petruk.furrymap.query._
import org.scalatest.{GivenWhenThen, Spec, FlatSpec}
import scala.PartialFunction

class OpenedIterableSelector[T] extends IterableSelector[T]{
  def getExpression = result
}

case class Awesome(name:String, age:Int, size:Double) extends Entity{
  def this() = this("",0,0)
}

object SetMatcher{
  def unapplySeq[T](item:Set[T]):Option[List[T]]=Some(item.toList)
}

@RunWith(classOf[JUnitRunner])
class QueryParsingSpec extends Spec with GivenWhenThen {

  class QueryValidator{
    def getSelectorFor[T <: Entity](implicit m: Manifest[T]) = new OpenedIterableSelector[T]

    def validate[T<:Entity](selector:OpenedIterableSelector[T])(matcher:PartialFunction[FBoolean,Unit]){
      if (!matcher.isDefinedAt(selector.getExpression)){
        fail(selector.getExpression+" does not match")
      }
    }
  }

  def fixture = new {
    val validator = new QueryValidator()
    val selector = validator.getSelectorFor[Awesome]
  }

  describe("Query evaluation"){
    it ("should parse age>10"){
      given("age>10 query")
      val f = fixture
      import f._
      selector.where(x=>x.age gt 10)
      then("it should match ")
      validator.validate(selector){
        case NumericBinaryOperation(IntFieldFNumeric(FieldInfo("age",_)),IntExactFNumeric(10),Greater)=>
      }
    }

    it ("should parse name === \'John\' "){
      given("name === \'John\' query")
      val f = fixture
      import f._
      selector.where(x=>x.name eqs "John")
      then("it should match ")
      validator.validate(selector){
        case StringBinaryOperation(FieldFString(FieldInfo("name",_)),ExactFString("John"),Equals)=>
      }
    }

    it ("should parse reversed expression 12.5 lt size"){
      given("12.5 lt size")
      val f = fixture
      import f._
      selector.where(x=> 12.5 lt x.size)
      then("it should match ")
      validator.validate(selector){
        case NumericBinaryOperation(DoubleExactFNumeric(12.5),DoubleFieldFNumeric(FieldInfo("size",_)),Less)=>
      }
    }

    it ("should parse age in (16, 18) and name eqs \'John\' or size gt 15.0"){
      given("age in (16, 18) and name eqs \'John\' or size gt 15.0 query")
      val f = fixture
      import f._
      selector.where(x=>(x.age in Set(16, 18)) && (x.name eqs "John") || (x.size gt 15.0))
      println(selector.result)
      then("it should match ")
      validator.validate(selector){
        case
          BooleanBinaryOperation(
            BooleanBinaryOperation(
              IntSetOperation(IntFieldFNumeric(FieldInfo(age,int)),SetMatcher(16,18)),
            StringBinaryOperation(
              FieldFString(FieldInfo("name",_)),ExactFString("John"),Equals),
            And),
              NumericBinaryOperation(DoubleFieldFNumeric(FieldInfo("size",_)),DoubleExactFNumeric(15.0),Greater),
            Or) =>
      }
    }
  }
}
