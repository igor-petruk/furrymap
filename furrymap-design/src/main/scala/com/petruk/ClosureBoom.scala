package com.petruk2

import java.lang.reflect.Method
import net.sf.cglib.proxy.{MethodProxy, MethodInterceptor, Enhancer}


/**
 * User: Igor Petruk
 * Date: 5/5/12
 * Time: 2:38 PM
 */

trait Selector{
  type K
}

case class FieldInfo(fieldName:String, fieldType:Class[_])
case class FieldNotificationException(info:FieldInfo) extends RuntimeException

class MyInterceptor extends MethodInterceptor{
  def intercept(obj: AnyRef, method: Method, args: Array[AnyRef], proxy: MethodProxy):AnyRef ={
    throw new FieldNotificationException(FieldInfo(method.getName, method.getReturnType))
  }
}

trait IterableSelector[B] extends Selector with Iterable[B]{
  type K=B
  def where(expression: K=>FBoolean)(implicit m: Manifest[K]):this.type = {
    val enhancer = new Enhancer();
    enhancer.setSuperclass(m.erasure);
    enhancer.setCallback(new MyInterceptor)
    val proxy = enhancer.create();
    val result = expression(proxy.asInstanceOf[K])
    println(result)
    this
  }

  def iterator:Iterator[K] = {
    println("Making iterator")
    List[K]().iterator
  }
}

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


abstract class MegaExpression

abstract class FBoolean extends MegaExpression{
  def &&(other:FBoolean):FBoolean = BooleanBinaryOperation(this, other, And)
  def ||(other:FBoolean):FBoolean = BooleanBinaryOperation(this, other, Or)
  def ===(other:FBoolean):FBoolean = BooleanBinaryOperation(this, other, Equals)
}

abstract class FNumeric extends MegaExpression{
  def gt(other:FNumeric):FBoolean = NumericBinaryOperation(this, other, Greater)
  def lt(other:FNumeric):FBoolean = NumericBinaryOperation(this, other, Less)
  def ===(other:FNumeric):FBoolean = NumericBinaryOperation(this, other, Equals)
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

abstract class FString extends MegaExpression with SetOperation[String]{
  def in(set: Iterable[String]) = StringSetOperation(this,  set)
  def like(other:FString):FBoolean = StringBinaryOperation(this, other, Like)
}
case class ExactFString(value:String) extends FString
case class FieldFString(field:FieldInfo) extends FString

object ops{

  implicit def conversionInt(i: =>Int):IntFNumeric=
    try{
      IntExactFNumeric(i)
    }catch{
      case e:FieldNotificationException=>return IntFieldFNumeric(e.info)
    }

  implicit def conversionDouble(i: =>Double):DoubleFNumeric=
    try{
      DoubleExactFNumeric(i)
    }catch{
      case e:FieldNotificationException=>return DoubleFieldFNumeric(e.info)
    }

  implicit def conversion(i: =>String):FString=
    try{
      ExactFString(i)
    }catch{
      case e:FieldNotificationException=>return FieldFString(e.info)
    }

}

trait EntityT{}

class Data(o: Object){
  def select[T <: EntityT](implicit m: Manifest[T])={
    new IterableSelector[T](){

    }
  }
}

class Awesome extends EntityT{
  val field1 = 0;
  val field2 = 0.1;
}

object ClosureBoom {
    val data = new Data(null)
    import data._
    import ops._

     def main(s:Array[String]){
        
        select[Awesome] where (x=>(x.field1 gt x.field2) && (x.field1 lt 19) || (x.field1 in Set(1,2,3)))
        //select[Awesome].where(x=>(x.field1>x.field2)&&(x.field2>2))
     }
}
