package group

import org.scalatest._

class AppTest extends FlatSpec with Matchers {

  "A Cache" should "evict the least recently used element if LRU policy is used" in {
    val obj = Cache[String, String](2, 0, "LRU")
    obj.insert("tanay", "great")
    obj.insert("hello", "there")
    assert(obj.getValue("tanay") == "great")
    obj.insert("nowAdded", "newValue")
    assert(obj.getValue("tanay") == "Element not found")
  }

  it should "evict the least frequently used element if LFU policy is used" in {
    val obj = Cache[String, String](2, 0, "LFU")
    obj.insert("tanay", "great")
    obj.insert("hello", "there")
    obj.insert("hello", "there")
    assert(obj.getValue("tanay") == "great")
    obj.insert("nowAdded", "newValue")
    assert(obj.getValue("tanay") == "Element not found")
  }

  it should "evict the most frequently used element if MFU policy is used" in {
    val obj = Cache[String, String](2, 0, "MFU")
    obj.insert("tanay", "great")
    obj.insert("hello", "there")
    obj.insert("hello", "there")
    assert(obj.getValue("hello") == "there")
    obj.insert("nowAdded", "newValue")
    assert(obj.getValue("hello") == "Element not found")
  }

  it should "evict the least string length element if LSL policy is used" in {
    val obj = Cache[String, String](2, 0, "LSL")
    obj.insert("tanay", "great")
    obj.insert("hi", "there")
    assert(obj.getValue("tanay") == "great")
    obj.insert("nowAdded", "newValue")
    assert(obj.getValue("hi") == "Element not found")
  }

  it should "evict the element which came first if FIFO policy is used" in {
    val obj = Cache[String, String](2, 0, "FIFO")
    obj.insert("tanay", "great")
    obj.insert("hello", "there")
    assert(obj.getValue("tanay") == "great")
    obj.insert("nowAdded", "newValue")
    assert(obj.getValue("tanay") == "Element not found")
  }
}
