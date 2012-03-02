import com.github.igor_petruk.furrymap.persistence.Entity
import com.mongodb.Mongo

/**
 * Created by IntelliJ IDEA.
 * User: boui
 * Date: 2/29/12
 * Time: 1:17 PM
 * To change this template use File | Settings | File Templates.
 */

object App {
  case class MyClass(name: String, age: Int) extends Entity {
    def this() = this("",0)
  }
  
  def main(args:Array[String]){
    val mongo = new Mongo
    val db = mongo.getDB("test");
    val myClassCollection = db.getCollection("MyClass")
    myClassCollection.setObjectClass(classOf[MyClass])
    myClassCollection.drop()
    val times = 1
    for (i<-0 until times){
      val myClass = MyClass("igor", 22);
      myClassCollection.insert(myClass)
    }
    val cursor = myClassCollection.find()
    while (cursor.hasNext){
      val next = cursor.next
      println(next)
    }

  }
}