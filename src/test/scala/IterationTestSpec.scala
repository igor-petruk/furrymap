
/**
 * Created by IntelliJ IDEA.
 * User: boui
 * Date: 3/1/12
 * Time: 8:23 PM
 * To change this template use File | Settings | File Templates.
 */

import collection.mutable.Stack
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers

@RunWith(classOf[JUnitRunner])
class IterationSpec extends FlatSpec with ShouldMatchers {

  "A Stack" should "pop values in last-in-first-out order" in {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    stack.pop() should equal(2)
    stack.pop() should equal(1)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[String]
    evaluating {
      emptyStack.pop()
    } should produce[NoSuchElementException]
  }
}
