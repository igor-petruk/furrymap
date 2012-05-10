package com.github.igor_petruk.furrymap.query

import java.lang.reflect.Method
import net.sf.cglib.proxy.{Enhancer, MethodProxy, MethodInterceptor}
import com.github.igor_petruk.furrymap.persistence.Entity
import com.mongodb.{BasicDBObject, Mongo, DBObject}
import java.util.ArrayList
import com.github.igor_petruk.furrymap.persistence._

/**
 * User: boui
 * Date: 5/6/12
 * Time: 4:09 PM
 */

object ToJava{
   def toJava[T](seq:Iterable[T]):ArrayList[T]={
    val l = new ArrayList[T]() 
    for(item<-seq){
      l.add(item)
    }
    l
   }
}

import ToJava._

case class FieldInfo(fieldName:String, fieldType:Class[_])
case class FieldNotificationException(info:FieldInfo) extends RuntimeException

abstract class OperationType{
  def abbreviation:String
}

object Equals extends OperationType{
  def abbreviation = null
}

abstract class NumericOperationType extends OperationType
object Greater extends NumericOperationType{
  def abbreviation = "$gt"
}
object Less extends NumericOperationType{
  def abbreviation = "$lt"
}

abstract class StringOperationType extends OperationType
object Like extends StringOperationType{
  def abbreviation = "$like"
}

abstract class BooleanOperationType extends OperationType
object And extends BooleanOperationType{
  def abbreviation = "$and"
}
object Or extends BooleanOperationType{
  def abbreviation = "$or"
}

trait SetOperation[T] {
  def in(set:Iterable[T]):FBoolean
}

case class FieldFBoolean(field:FieldInfo) extends FBoolean with Evaluatable{
  def eval = new BasicDBObject(field.fieldName, true)
}
case class TrueFBoolean() extends FBoolean{
  def eval = new BasicDBObject()
}
case class BooleanBinaryOperation(left:FBoolean,  right:FBoolean, op:OperationType) extends FBoolean{
  def eval = op match {
    case And|Or=> new BasicDBObject(op.abbreviation, toJava(List(left.eval, right.eval)))
  }
}
case class NumericBinaryOperation(left:FieldFNumeric,  right:ExactFNumeric, op:OperationType) extends FBoolean with Evaluatable{
  def eval = op match {
    case Greater|Less => new BasicDBObject(left.getName, new BasicDBObject(op.abbreviation, right.getValue))
  }
}
case class IntSetOperation(left:IntFieldFNumeric,  right:Iterable[Int]) extends FBoolean with Evaluatable{
  def eval = new BasicDBObject(left.getName, new BasicDBObject("$in", toJava(right)))
}
case class DoubleSetOperation(left:DoubleFieldFNumeric,  right:Iterable[Double]) extends FBoolean with Evaluatable{
  def eval = new BasicDBObject(left.getName, new BasicDBObject("$in", toJava(right)))
}
case class StringBinaryOperation(left:FieldFString,  right:ExactFString, op:OperationType) extends FBoolean with Evaluatable{
  def eval = op match {
    case Equals=> new BasicDBObject(left.field.fieldName, right.value)
    case Like=> new BasicDBObject(left.field.fieldName, new BasicDBObject(op.abbreviation, right.value))
  }
}
case class StringSetOperation(left:FieldFString,  right:Iterable[String]) extends FBoolean with Evaluatable {
  def eval = new BasicDBObject(left.field.fieldName, new BasicDBObject("$in", toJava(right)))
}


abstract class FExpression{
}

trait Evaluatable{self:FBoolean=>
  def eval:DBObject
}

abstract class FBoolean extends FExpression with Evaluatable{
  def &&(other:FBoolean):FBoolean = BooleanBinaryOperation(this, other, And)
  def ||(other:FBoolean):FBoolean = BooleanBinaryOperation(this, other, Or)
  def eqs(other:FBoolean):FBoolean = BooleanBinaryOperation(this, other, Equals)
}

abstract class FNumeric extends FExpression{
}

trait ExactFNumeric {
  def getValue:AnyRef
}

trait FieldFNumeric {
  def getName:String
  def gt(other:ExactFNumeric):FBoolean = NumericBinaryOperation(this, other, Greater)
  def lt(other:ExactFNumeric):FBoolean = NumericBinaryOperation(this, other, Less)
  def eqs(other:ExactFNumeric):FBoolean = NumericBinaryOperation(this, other, Equals)
}

abstract class DoubleFNumeric extends FNumeric
case class DoubleExactFNumeric(value:Double) extends DoubleFNumeric with ExactFNumeric{
  def getValue = value.asInstanceOf[AnyRef]
}
case class DoubleFieldFNumeric(field:FieldInfo) extends DoubleFNumeric with FieldFNumeric with SetOperation[Double]{
  def in(set: Iterable[Double]) = DoubleSetOperation(this, set)
  def getName = field.fieldName
}

abstract class IntFNumeric extends FNumeric

case class IntExactFNumeric(value:Int) extends IntFNumeric with ExactFNumeric{
  def getValue = value.asInstanceOf[AnyRef]
}
case class IntFieldFNumeric(field:FieldInfo) extends IntFNumeric with FieldFNumeric with SetOperation[Int]{
  def in(set: Iterable[Int]) = IntSetOperation(this, set)
  def getName = field.fieldName
}

abstract class FString extends FExpression

case class ExactFString(value:String) extends FString
case class FieldFString(field:FieldInfo) extends FString with SetOperation[String] {
  def in(set: Iterable[String]) = StringSetOperation(this,  set)
  def like(other:ExactFString):FBoolean = StringBinaryOperation(this, other, Like)
  def eqs(other:ExactFString):FBoolean = StringBinaryOperation(this, other, Equals)
}

trait Selector{
  type K
}

class MyInterceptor(klass:Class[_]) extends MethodInterceptor{
  def intercept(obj: AnyRef, method: Method, args: Array[AnyRef], proxy: MethodProxy):AnyRef ={
    try{
      klass.getDeclaredField(method.getName)
      throw new FieldNotificationException(FieldInfo(method.getName, method.getReturnType))
    }catch{
      case e:NoSuchFieldException=>proxy.invokeSuper(obj, args)
    }
  }
}

class IterableSelector[B<:Entity](database:MongoDB)(implicit m:Manifest[B]) extends Selector with Iterable[B]{
  type K=B

  var expression:FBoolean = _

  def where(queryExpression: K=>FBoolean)(implicit m: Manifest[K]):this.type = {
    val enhancer = new Enhancer();
    enhancer.setSuperclass(m.erasure);
    enhancer.setCallback(new MyInterceptor(m.erasure))
    val proxy = enhancer.create();
    expression = queryExpression(proxy.asInstanceOf[K])
    this
  }

  def all():this.type = {
    expression = TrueFBoolean()
    this
  }

  def getExpression = expression

  private def runQuery={
    val collection = database.getCollection(m.erasure)
    val cursor = collection.find(expression.eval)

    var list:List[K] = Nil
    while(cursor.hasNext){
      list = cursor.next().asInstanceOf[K]::list
    }
    cursor.close
    list
  }

  private lazy val queryResults = runQuery

  def iterator:Iterator[K] = queryResults.iterator
}
