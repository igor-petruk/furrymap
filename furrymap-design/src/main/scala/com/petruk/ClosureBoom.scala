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
    null
  }
}

trait IterableSelector[B] extends Selector with Iterable[B]{
  type K=B
  def where(expression: K=>MegaBoolean)(implicit m: Manifest[K]):this.type = {
    val enhancer = new Enhancer();
    enhancer.setSuperclass(m.erasure);
    enhancer.setCallback(new MyInterceptor)
    val proxy = enhancer.create();
    val result:MegaBoolean = expression(proxy.asInstanceOf[K])
    println(result)
    this
  }

  def iterator:Iterator[K] = {
    println("Making iterator")
    List[K]().iterator
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
  val field2 = 0;
}

abstract class MegaExpression


abstract class MegaBoolean extends MegaExpression{
  def &&(other:MegaBoolean):MegaBoolean = AndMegaBoolean(this, other)
}
case class ExactMegaBoolean(value:Boolean) extends MegaBoolean
case class FieldMegaBoolean(field:FieldInfo) extends MegaBoolean
case class AndMegaBoolean(left:MegaBoolean, right:MegaBoolean)  extends MegaBoolean
case class GreaterNumericsMegaBoolean(left:MegaNumeric, right:MegaNumeric)  extends MegaBoolean


abstract class MegaNumeric extends MegaExpression{
  def !>(other:MegaNumeric):MegaBoolean = GreaterNumericsMegaBoolean(this, other)
}
case class ExactMegaNumeric(value:Int) extends MegaNumeric
case class FieldMegaNumeric(field:FieldInfo) extends MegaNumeric

object ops{

  implicit def i2mn[K](i: =>K)(implicit m:Manifest[K]):MegaNumeric={
    try{
      val value:Int = i.asInstanceOf[Int]
      return ExactMegaNumeric(value)
    }catch{
      case e:FieldNotificationException=>return FieldMegaNumeric(e.info)
    }
  }

}

object ClosureBoom {
    val data = new Data(null)
    import data._
    import ops._

     def main(s:Array[String]){
        
        select[Awesome] where (x=>(x.field1!>x.field2) && (x.field1!>19))
        //select[Awesome].where(x=>(x.field1>x.field2)&&(x.field2>2))
     }
}
