package com.github.igor_petruk.furrymap.dbobject

import collection.immutable
import java.lang.reflect.Field
import collection.mutable.{SynchronizedMap, HashMap}
import com.mongodb.DBObject
import org.bson.BSONObject
import org.bson.types.ObjectId

/**
 * User: boui
 * Date: 5/6/12
 * Time: 3:58 PM
 */

object EntitiesMetaInfo {

  class MetaInfo(klass: Class[_]) {

    /**
     * This is a list of mappings of fields, that
     * have different name in class, then in MongoDB collection.
     * First item is name in collection, second one is name in class
     * @return mappings
     */
    def specialMapping = immutable.Map[String, String](
      "_id" -> "objectId"
    )

    /**
     * Calculate fields that are mapped with special mappings
     * @return
     */
    def extraFields: immutable.Map[String, Field] =
      specialMapping.
        filter {
        item => klass.getDeclaredField(item._2) != null
      }.
        map {
        item => (item._1, klass.getDeclaredField(item._2))
      }


    /**
     * Precalcuated mapping of fields in collections to fields
     * in class
     */
    val fields: immutable.Map[String, Field] = provideFields;

    private def provideFields = {
      def makeTuple(f: Field) = (f.getName, f)
      def makeAccesible(item: (String, Field)) = {
        item._2.setAccessible(true) // Access to private, final...
        item
      }

      (klass.getDeclaredFields.
        filterNot(shouldBeAdded).
        map(makeTuple)
        ++
        extraFields).
        map(makeAccesible).
        toMap[String, Field]
    }

    /**
     * Filter out field if it should not be persisted
     * or should be processed via extraFields
     * @param field
     * @return
     */
    private def shouldBeAdded(field: Field) = {
      (specialMapping.values.exists(field.getName == _)) ||
        (field.getName.contains("metaInfo")) ||
        (field.getName.contains("furrymapObjectInfo"))
    }
  }

  /**
   * Info on entity (mappings, etc).
   */
  @transient
  private val metainfo = new HashMap[Class[_], MetaInfo] with SynchronizedMap[Class[_], MetaInfo]

  /**
   * Accessor and lazy populator of metainfo
   * @param klass class to query
   * @tparam T
   * @return metainfo
   */
  def getMetaInfo[T](klass: Class[T]) = metainfo.getOrElseUpdate(klass, new MetaInfo(klass))
}

class DBObjectInfo {
  /**
   * Tells if object is partially populated via partial query
   */
  var partialObject = false;
}

trait DBObjectImpl extends DBObject {

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
    for (entity <- m) {
      put(entity._1.asInstanceOf[String], entity._2.asInstanceOf[AnyRef])
    }
  }

  @transient
  private val metaInfo = getMetaInfo(getClass)

  def get(key: String) = metaInfo.fields.get(key).map(_.get(this)).getOrElse(null)

  def put(key: String, v: AnyRef) = {
    for (field <- metaInfo.fields.get(key))
      field.set(this, v)
    v
  }

  import scala.collection.JavaConversions._

  def toMap = {
    val map = new java.util.HashMap[AnyRef, AnyRef]
    for (key <- keySet()) {
      map.put(key, get(key))
    }
    map
  }

  def removeField(key: String) = null

  def containsKey(s: String) = containsField(s)

  def containsField(s: String) = metaInfo.fields.containsKey(s)

  def keySet() = metaInfo.fields.keySet
}

/**
 * Trait that gives access to object id
 */
trait WithObjectId {
  var objectId: ObjectId = new ObjectId()
}

