package com.github.igor_petruk.furrymap

import dbobject._
import query._
import com.mongodb.{DB, Mongo}

/**
 * User: Igor Petruk
 * Date: 01.03.12
 * Time: 22:33
 */

private trait QueryImplicits{
  implicit def conversionInt(i: =>Int):IntFNumeric=
    try{
      IntExactFNumeric(i)
    }catch{
      case e:
        FieldNotificationException=>return IntFieldFNumeric(e.info)
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

object persistence
  extends QueryImplicits{

  trait Entity extends WithObjectId with DBObjectImpl

  class MongoDB private[persistence] (db:DB){
    def isConnected = db.getMongo.getConnector.isOpen

    def select[T <: Entity](implicit m: Manifest[T]) = new IterableSelector[T](){}
  }

  class Furrymap  private(mongo:Mongo) {
    def getDatabase(name:String) = new MongoDB(mongo.getDB(name))

    def close = mongo.close()
  }

  object Furrymap{
    def localMongo = new Furrymap(new Mongo)
  }

}