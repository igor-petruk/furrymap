package com.github.igor_petruk.furrymap

import dbobject._
import query._
import collection.mutable.{SynchronizedMap, HashMap}
import com.mongodb.{BasicDBObject, DBCollection, DB, Mongo}

/**
 * User: Igor Petruk
 * Date: 01.03.12
 * Time: 22:33
 */

trait QueryImplicits{
  private def complain = throw new IllegalStateException("Usage of operation with numbers in first position is not allowed")
  private def complainToField = throw new IllegalStateException("Usage of operation with field in second position is not allowed")

  implicit def conversionInt1(i: =>Int):IntFieldFNumeric=
    try{
      val k:Int = i
      complain
    }catch{
      case e:
        FieldNotificationException=>return IntFieldFNumeric(e.info)
    }

  implicit def conversionInt2(i: =>Int):IntExactFNumeric=
    try{
      IntExactFNumeric(i)
    }catch{
      case e:
        FieldNotificationException=> complainToField
    }


  implicit def conversionDouble1(i: =>Double):DoubleFieldFNumeric=
    try{
      val k = i
      complain
    }catch{
      case e:FieldNotificationException=>return DoubleFieldFNumeric(e.info)
    }

  implicit def conversionDouble2(i: =>Double):DoubleExactFNumeric=
    try{
      DoubleExactFNumeric(i)
    }catch{
      case e:FieldNotificationException=>complainToField
    }

  implicit def conversion1(i: =>String):FieldFString=
    try{
      val k = i
      complain
    }catch{
      case e:FieldNotificationException=>return FieldFString(e.info)
    }

  implicit def conversion2(i: =>String):ExactFString=
    try{
      ExactFString(i)
    }catch{
      case e:FieldNotificationException=>complainToField
    }
}

object persistence
  extends QueryImplicits{

  trait Entity extends WithObjectId with DBObjectImpl {
    def collectionName = this.getClass.getName
  }

  class MongoDB private[persistence] (db:DB){
    import scala.collection.JavaConversions._

    def isConnected = db.getMongo.getConnector.isOpen

    def select[T <: Entity](implicit m: Manifest[T]) = new IterableSelector[T](this)

    def insert[T <: Entity](entities: T*)(implicit m: Manifest[T]){
      for ((groupName, items)<-entities.groupBy(_.collectionName)){
        val collection = db.getCollection(groupName)
        collection.insert(items)
      }
    }
    
    def index[T<:Entity](fields: (T=>Any)*)(implicit m:Manifest[T]):this.type={
      val indexObject = new BasicDBObject()
      for (field<-fields){
        try{
          val entity:T = InterceptorBuilder.buildInterceptor("",m.erasure).asInstanceOf[T]
          val value = field(entity)
        }catch{
          case e:FieldNotificationException=>{
            indexObject.put(e.info.fieldName, 1)
          }
        }
      }
      getCollection(m.erasure).ensureIndex(indexObject)
      this
    }

    def dropCollection[T<:Entity](implicit m:Manifest[T]){
      dropCollection(m.erasure.getName)
    }

    def dropCollection(name:String){
      db.getCollection(name).drop()
    }

    def getDatabaseObject = db

    private val collections = new HashMap[Class[_], DBCollection] with SynchronizedMap[Class[_], DBCollection]
    def getCollection(klass:Class[_])=collections.getOrElseUpdate(klass, {
      val collection = db.getCollection(klass.getName)
      EntitiesMetaInfo.prepareClassesConfig(klass, collection)
      collection
    })
  }

  class Furrymap  private(mongo:Mongo) {
    def getDatabase(name:String) = new MongoDB(mongo.getDB(name))

    def close = mongo.close()
  }

  object Furrymap{
    def localMongo = new Furrymap(new Mongo)
  }

}