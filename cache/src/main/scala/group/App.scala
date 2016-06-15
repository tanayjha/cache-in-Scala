package group

case class Cache[Key, Value](cacheCapacity: Int, var cacheCurrentSize: Int, PolicyUsed: String) {

  var cache: List[Key Tuple2 Value] = List()
  var allElements: List[Key Tuple2 Value] = List()


  def insert(element: Key Tuple2 Value) = {
    PolicyUsed match {
      case "LRU" => evictionPolicyLRU(element)

      case "LFU" => evictionPolicyLFU(element)

      case "MFU" => evictionPolicyMFU(element)

      case "LSL" => evictionPolicyLSL(element)

      case "FIFO" => evictionPolicyFIFO(element)
    }
  }

  def insertTuple(element: Key Tuple2 Value): Boolean = {
    allElements = allElements :+ element
    if(cache.contains(element)) {
      return true
    }
    if(cacheCurrentSize < cacheCapacity) {
      cacheCurrentSize += 1
      cache = element :: cache
      true
    }
    else {
      false
    }
  }

  def getValue(element: Key): Value = {
    if(cache.exists(_._1 == element)) {
      cache.find(_._1 == element).get._2
    }
    else {
      "Element not found".asInstanceOf[Value]
    }
  }

  def remove(delElement: Key Tuple2 Value) = {
    if(cache.contains(delElement)) {
      val tempCache = cache.filter(_ != delElement)
      cache = tempCache
      cacheCurrentSize -= 1
    }
    else {
      println("The element is not present in the cache")
    }
  }

  def evictionPolicyLRU(element: Key Tuple2 Value) = {
    if(cache.contains(element)) {
      remove(element)
      insertTuple(element)
    }
    else {
      if(cacheCurrentSize < cacheCapacity) {
        insertTuple(element)
      }
      else {
        remove(cache.last)
        insertTuple(element)
      }
    }
  }

  def evictionPolicyLSL(element: Key Tuple2 Value) = {
    if(!cache.contains(element)) {
      if(cacheCurrentSize < cacheCapacity) {
        insertTuple(element)
      }
      else {
        val delElement = cache.sortBy(_._1.asInstanceOf[String].length).head
        remove(delElement)
        insertTuple(element)
      }
    }
    else {
      remove(element)
      insertTuple(element)
    }
  }

  def evictionPolicyLFU(element: Key Tuple2 Value) = {
    if(!cache.contains(element)) {
      if(cacheCurrentSize < cacheCapacity) {
        insertTuple(element)
      }
      else {
        val delElement = cache.sortBy(x => allElements.count(_ == x)).head
        remove(delElement)
        insertTuple(element)
      }
    }
    else {
      remove(element)
      insertTuple(element)
    }
  }

  def evictionPolicyMFU(element: Key Tuple2 Value) = {
    if(!cache.contains(element)) {
      if(cacheCurrentSize < cacheCapacity) {
        insertTuple(element)
      }
      else {
        val delElement = cache.sortBy(x => allElements.count(_ == x)).last
        remove(delElement)
        insertTuple(element)
      }
    }
    else {
      remove(element)
      insertTuple(element)
    }
  }

  def evictionPolicyFIFO(element: Key Tuple2 Value) = {
    if(cache.contains(element)) {
      remove(element)
      insertTuple(element)
    }
    else {
      if(cacheCurrentSize < cacheCapacity) {
        insertTuple(element)
      }
      else {
        val delElement = cache.sortBy(x => allElements.indexOf(x)).head
        remove(delElement)
        insertTuple(element)
      }
    }
  }
}

