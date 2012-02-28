package com.petruk

import java.lang.reflect.{Method, InvocationHandler}
import net.sf.cglib.proxy.{Enhancer, MethodProxy, MethodInterceptor}

trait Entity{}

object NumericOperation extends Enumeration{
  type NumericOperation = Value
  val Greater = Value
}

import NumericOperation._

abstract class Instruction
case class Field(name:String, klass: Class[_]) extends Instruction
case class OperationForInt(operation:NumericOperation, value:Int) extends Instruction

case class Instructions(instructions:List[Instruction])

object InstructionHolder{
  private val holder = new ThreadLocal[Instructions]();
  def reset(){
    holder.remove();
  }

  private def getObject={
    if (holder.get()==null){
      val i = new Instructions(List[Instruction]())
      holder.set(i)
      i
    }else{
      holder.get();
    }
  }

  def get = getObject.instructions

  def add(i:Instruction){
    holder.set(Instructions(i :: getObject.instructions))
  }
}

class Operation(i:Int){
  def !>(k:Int) = {
    InstructionHolder.add(OperationForInt(NumericOperation.Greater, k))
    true
  }
}

class MyInterceptor extends MethodInterceptor{
  def intercept(obj: AnyRef, method: Method, args: Array[AnyRef], proxy: MethodProxy):AnyRef ={
    InstructionHolder.add(Field(method.getName, method.getReturnType))
    null//method.invoke(obj, args);
  }
}

trait Selector{
  type K
  def where(expression: K=>Boolean)(implicit m: Manifest[K]):List[K] = {
    if (m.erasure==classOf[Awesome]){
      val enhancer = new Enhancer();
      enhancer.setSuperclass(classOf[Awesome]);
      enhancer.setCallback(new MyInterceptor)
      val proxy = enhancer.create();
      InstructionHolder.reset()
      expression(proxy.asInstanceOf[K])
      println("Ready for query translations got")
      for (k<-InstructionHolder.get.reverse){
        println(k)
      }
    }
    return List[K]()
  }
}

class Data(o: Object){
   def select[T <: Entity](implicit m: Manifest[T])={
      new Selector{type K = T}
   }
}

class Awesome extends Entity{
  val field1 = 0;
  val field2 = 0;
}

object App 
{
  implicit def i2c(i:Int):Operation = new Operation(i)

    def main(args: Array[String]):Unit={
        val data = new Data(null)
        import data._
       select[Awesome] where {x=>x.field1 !> x.field2}
    }
}
