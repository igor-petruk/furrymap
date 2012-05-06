package com.github.igor_petruk.furrymap.query

import java.lang.reflect.Method
import net.sf.cglib.proxy.{Enhancer, MethodProxy, MethodInterceptor}

/**
 * User: boui
 * Date: 5/6/12
 * Time: 4:09 PM
 */

case class FieldInfo(fieldName:String, fieldType:Class[_])
case class FieldNotificationException(info:FieldInfo) extends RuntimeException

abstract class OperationType

object Equals extends OperationType

abstract class NumericOperationType extends OperationType
object Greater extends NumericOperationType
object Less extends NumericOperationType

abstract class StringOperationType extends OperationType
object Like extends StringOperationType

abstract class BooleanOperationType extends OperationType
object And extends BooleanOperationType
object Or extends BooleanOperationType

trait SetOperation[T] {
  def in(set:Iterable[T]):FBoolean
}

case class ExactFBoolean(value:Boolean) extends FBoolean
case class FieldFBoolean(field:FieldInfo) extends FBoolean
case class BooleanBinaryOperation(left:FBoolean,  right:FBoolean, op:OperationType) extends FBoolean
case class NumericBinaryOperation(left:FNumeric,  right:FNumeric, op:OperationType) extends FBoolean
case class IntSetOperation(left:IntFNumeric,  right:Iterable[Int]) extends FBoolean
case class DoubleSetOperation(left:DoubleFNumeric,  right:Iterable[Double]) extends FBoolean
case class StringBinaryOperation(left:FString,  right:FString, op:OperationType) extends FBoolean
case class StringSetOperation(left:FString,  right:Iterable[String]) extends FBoolean


abstract class FExpression

abstract class FBoolean extends FExpression{
  def &&(other:FBoolean):FBoolean = BooleanBinaryOperation(this, other, And)
  def ||(other:FBoolean):FBoolean = BooleanBinaryOperation(this, other, Or)
  def eqs(other:FBoolean):FBoolean = BooleanBinaryOperation(this, other, Equals)
}

abstract class FNumeric extends FExpression{
  def gt(other:FNumeric):FBoolean = NumericBinaryOperation(this, other, Greater)
  def lt(other:FNumeric):FBoolean = NumericBinaryOperation(this, other, Less)
  def eqs(other:FNumeric):FBoolean = NumericBinaryOperation(this, other, Equals)
}

abstract class DoubleFNumeric extends FNumeric with SetOperation[Double]{
  def in(set: Iterable[Double]) = DoubleSetOperation(this, set)
}
case class DoubleExactFNumeric(value:Double) extends DoubleFNumeric
case class DoubleFieldFNumeric(field:FieldInfo) extends DoubleFNumeric

abstract class IntFNumeric extends FNumeric  with SetOperation[Int]{
  def in(set: Iterable[Int]) = IntSetOperation(this, set)
}
case class IntExactFNumeric(value:Int) extends IntFNumeric
case class IntFieldFNumeric(field:FieldInfo) extends IntFNumeric

abstract class FString extends FExpression with SetOperation[String]{
  def in(set: Iterable[String]) = StringSetOperation(this,  set)
  def like(other:FString):FBoolean = StringBinaryOperation(this, other, Like)
  def eqs(other:FString):FBoolean = StringBinaryOperation(this, other, Equals)
}
case class ExactFString(value:String) extends FString
case class FieldFString(field:FieldInfo) extends FString

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

trait IterableSelector[B] extends Selector with Iterable[B]{
  type K=B

  var result:FBoolean = _

  def where(expression: K=>FBoolean)(implicit m: Manifest[K]):this.type = {
    val enhancer = new Enhancer();
    enhancer.setSuperclass(m.erasure);
    enhancer.setCallback(new MyInterceptor(m.erasure))
    val proxy = enhancer.create();
    result = expression(proxy.asInstanceOf[K])
    this
  }

  def iterator:Iterator[K] = {
    println("Making iterator")
    List[K]().iterator
  }
}
