package com.github.igor_petruk.furrymap

import net.sf.cglib.proxy.Enhancer
import org.bson.types.ObjectId
import com.mongodb.DBObject
import org.bson.BSONObject
import com.github.igor_petruk.furrymap.persistence.DBObjectInfo

import collection.mutable.{SynchronizedMap, HashMap}
import com.github.igor_petruk.furrymap.persistence.EntitiesMetaInfo.MetaInfo
import collection.immutable.MapLike
import scala.Option
import java.util.Map

import scala.collection._
import java.lang.reflect.{Modifier, Field}

/**
 * User: Igor Petruk
 * Date: 01.03.12
 * Time: 22:33
 */

object persistence {
  object EntitiesMetaInfo{
    class MetaInfo(klass: Class[_]){
      
      def specialMapping=immutable.Map[String, String](
        "_id"->"objectId"
      )
      
      def extraFields:immutable.Map[String,Field]=
        specialMapping.
          filter{item=>klass.getDeclaredField(item._2)!=null}.
          map{item=>(item._1, klass.getDeclaredField(item._2))}

    
      val fields:immutable.Map[String, Field] = provideFields;
      private def provideFields=
        (klass.getDeclaredFields.
            filterNot(shouldBeAdded).
            map({x=>(x.getName, x)})
          ++
            extraFields
        ).map(item=>{
            item._2.setAccessible(true)
            item
        }).
        toMap[String,Field]
      
      private def shouldBeAdded(field:Field)={
        (specialMapping.values.exists(field.getName==_))||
        (field.getName.contains("metaInfo"))||
        (field.getName.contains("furrymapObjectInfo"))
      }
    }
    @transient
    private val metainfo = new HashMap[Class[_], MetaInfo] with SynchronizedMap[Class[_],MetaInfo]
    
    def getMetaInfo[T](klass: Class[T]) = metainfo.getOrElseUpdate(klass,new MetaInfo(klass))
  }
  
  class DBObjectInfo {
    var partialObject = false;
  }

  trait DBObjectImpl extends DBObject{
    import EntitiesMetaInfo._
    
    @transient
    private val furrymapObjectInfo = new DBObjectInfo()
    private def objectInfo = furrymapObjectInfo
    
    def markAsPartialObject() {
      furrymapObjectInfo.partialObject = true
    }

    def isPartialObject = furrymapObjectInfo.partialObject

    def putAll(o: BSONObject) {
      putAll(o.toMap)
    }

   def putAll(m: Map[_, _]) {
     import scala.collection.JavaConversions._
     for (entity <- m){
        put(entity._1.asInstanceOf[String], entity._2.asInstanceOf[AnyRef])
      }
    }

    @transient
    private val metaInfo = getMetaInfo(getClass)

    def get(key: String) = metaInfo.fields.get(key).map(_.get(this)).getOrElse(null)

    def put(key: String, v: AnyRef)={
      for (field<-metaInfo.fields.get(key))
          field.set(this, v)
      v
    }

    import scala.collection.JavaConversions._

    def toMap = {
      val map = new java.util.HashMap[AnyRef, AnyRef]
      for (key <- keySet()){
        map.put(key, get(key))
      }
      map
    }

    def removeField(key: String)=null

    def containsKey(s: String) = containsField(s)

    def containsField(s: String) = metaInfo.fields.containsKey(s)

    def keySet() = metaInfo.fields.keySet
  }
  
  trait WithObjectId{
    var objectId: ObjectId = new ObjectId()
  }
  
  trait Entity extends WithObjectId with DBObjectImpl

}
