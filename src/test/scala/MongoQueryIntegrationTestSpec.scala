/**
 * User: Igor Petruk
 * Date: 5/7/12
 * Time: 7:28 PM
 */
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import com.github.igor_petruk.furrymap.persistence._
import org.scalatest.{GivenWhenThen, Spec}

case class Person(name:String, age:Int, height:Double, alive:Boolean) extends Entity{
  def this() = this("",0,0, false)
}

case class Game(name:String,player1:Person, player2:Person) extends Entity{
  def this() = this("hello",null, null)
}

@RunWith(classOf[JUnitRunner])
class MongoQueryIntegrationTestSpec extends Spec with GivenWhenThen {
  val db = Furrymap.localMongo.getDatabase("test")
  import db._

  val items = List(
    Person("Igor", 22, 190, true),
    Person("Rocksy", 21, 180, true)
  )

  val games = List(
    Game("Chess",items(0),items(1)),
    Game("Cards",items(1),items(0))
  )

  def givenAFixture{
    given(items.toString())
    dropCollection[Person]
    insert(items:_*)
    dropCollection[Game]
    insert(games:_*)
  }

  describe("FurryMap API"){
    it ("should support eqs operation"){
      givenAFixture
      when("select name eqs \"Rocksy\" is performed")
        val result = select[Person] where (_.name eqs "Rocksy")
      then("it should return " + items(1))
        assert(result.head === items(1))
        assert(result.size === 1)
    }
    it ("should support storing embedded objects"){
      givenAFixture
      when("select of all Game's is performed")
      val result = select[Game].all()
      then("it should return " + games)
      assert(result.exists (_==games(0)))
      assert(result.exists (_==games(1)))
      assert(result.size === 2)
    }
  }
}
