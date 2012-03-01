package com.petruk

import java.lang.reflect.{Method, InvocationHandler}
import net.sf.cglib.proxy.{Enhancer, MethodProxy, MethodInterceptor}
import collection.JavaConversions._
import com.mongodb.{BasicDBObject, Mongo}

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

object TransactionIsolation extends Enumeration{
  type TransactionIsolation = Value
  val READ_UNCOMMITED = Value
}

import TransactionIsolation._

class TransactionFlags
object ForceNew extends TransactionFlags
object SomeOther extends TransactionFlags

object transaction{
  def apply
  (flags:TransactionFlags*)
  (f: => Unit){
    f
  }
  def apply (f: => Unit):Unit = apply()(f)
}

trait Selector{
  type K
}

trait IterableSelector[B] extends Selector with Iterable[B]{
  type K=B
  def where(expression: K=>Boolean)(implicit m: Manifest[K]):this.type = {
    println(m.erasure)
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
    this
  }

  def only(queries: (K=>Any)*)(implicit m: Manifest[K]):this.type={
    println(m.erasure)
    for (q<-queries){
      q(null.asInstanceOf[K])
    }
    this
  }

  def async(f: Iterable[K]=>Any){
    f(null)
  }

  def iterator:Iterator[K] = {
    println("Making iterator")
    List[K]().iterator
  }
}

class Data(o: Object){
   def select[T <: Entity](implicit m: Manifest[T])={
      new IterableSelector[T](){
        
      }
   }
}

class Awesome extends Entity{
  val field1 = 0;
  val field2 = 0;
}

case class Elem(i:Boolean){
  println("Elem")
  def !&(that:Elem)={
    println("and")
    BinaryOp(true)
  }
}
case class BinaryOp(b:Boolean){
  println("binaryop "+b)
}

object App 
{
  def code(b: BinaryOp){

  }

  implicit def b2e(b:Boolean)=Elem(b)
  implicit def b2b(b:Boolean)=BinaryOp(b)

  implicit def i2c(i:Int):Operation = new Operation(i)

  implicit def sel2it[T](i: Selector{type K=T}):Iterable[T]=null

    def main(args: Array[String]):Unit={
       code(true !& false)
        val data = new Data(null)
        import data._
       
      val k = select[Awesome].only(_.field1).where(x=>x.field1 !> 0).map(2*_.field1)
      select[Awesome].only(_.field1).where(x=>x.field1 !> 0).async{ result=>
        println(result.head.field1)
      }

      transaction(SomeOther, ForceNew){
        transaction(){

        }
      }

      val mongo = new Mongo();
      val db = mongo.getDB("furrymap-design");
      for (collection<-db.getCollectionNames)
        println(collection)
      
      val collection = db.getCollection("collection1");
      collection.drop();

      for (i<-0 to 1000000){
        val obj = new BasicDBObject();
        obj.put("test", "some");
        collection.insert(obj)
      }


      /*for(a<-asScalaIterator(collection.find())){
        println(a)
      } */
      
    }
}
