package group

import java.util

import org.scalatest._

class AppTest extends FlatSpec with Matchers {
  "A Cache" should "not insert values after it is full unless an eviction policy is used" in {
    var obj = new Test(2, 0)
    assert(obj.insertInCache("tanay") == "Element added")
    assert(obj.insertInCache("hello") == "Element added")
    assert(obj.insertInCache("overflow") == "Cache is full")
  }

  it should "evict the least recently used element if LRU policy is used" in {
    var obj = new Test(2, 0)
    obj.insertInCache("tanay")
    obj.insertInCache("hello")
    assert(obj.getElementPolicyLRU("nowAdded") == List("nowAdded", "hello"))
  }

  it should "evict the least frequently used element if LFU policy is used" in {
    var obj = new Test(2, 0)
    obj.insertInCache("tanay")
    obj.insertInCache("hello")
    obj.insertInCache("hello")
    assert(obj.getElementPolicyLFU("nowAdded") == List("nowAdded", "hello"))
  }

  it should "evict the most frequently used element if MFU policy is used" in {
    var obj = new Test(2, 0)
    obj.insertInCache("tanay")
    obj.insertInCache("hello")
    obj.insertInCache("hello")
    assert(obj.getElementPolicyMFU("nowAdded") == List("nowAdded", "tanay"))
  }

  it should "evict the least string length element if LSL policy is used" in {
    var obj = new Test(2, 0)
    obj.insertInCache("tanay")
    obj.insertInCache("hi")
    assert(obj.getElementPolicyLSL("nowAdded") == List("nowAdded", "tanay"))
  }

  it should "evict the element which came first if FIFO policy is used" in {
    var obj = new Test(2, 0)
    obj.insertInCache("tanay")
    obj.insertInCache("hi")
    assert(obj.getElementPolicyFIFO("nowAdded") == List("nowAdded", "hi"))
  }
}
