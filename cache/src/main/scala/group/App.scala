package group


case class Cache[Key, Value](cacheCapacity: Int, var cacheCurrentSize: Int, PolicyUsed: String) {

  var cache: List[Tuple2[Key, Value]] = List()
  var allElements: List[Tuple2[Key, Value]] = List()


  def insert(element: Tuple2[Key, Value]) = {
    PolicyUsed match {
      case "LRU" => getElementPolicyLRU(element)

      case "LFU" => getElementPolicyLFU(element)

      case "MFU" => getElementPolicyMFU(element)

      case "LSL" => getElementPolicyLSL(element)

      case "FIFO" => getElementPolicyFIFO(element)
    }
  }

  def insertTuple(element: Tuple2[Key, Value]): Boolean = {
    allElements = allElements :+ element
    if(cache.contains(element)) {
      return true
    }
    if(cacheCurrentSize < cacheCapacity) {
      cacheCurrentSize += 1
      cache = element :: cache
      return true
    }
    else {
      return false
    }
  }

  def getValue(element: Key): Value = {
    if(cache.find(_._1 == element) != None)
      return cache.find(_._1 == element).get._2
    return "Element not found".asInstanceOf[Value]
  }

  def remove(delElement: Tuple2[Key, Value]) = {
    if(cache.contains(delElement)) {
      val tempCache = cache.filter(_ != delElement)
      cache = tempCache
      cacheCurrentSize -= 1
    }
    else {
      println("The element is not present in the cache")
    }
  }

  def getElementPolicyLRU(getElement: Tuple2[Key, Value]) = {
    if(cache.contains(getElement)) {
      remove(getElement)
      insertTuple(getElement)
    }
    else {
      if(cacheCurrentSize < cacheCapacity) {
        insertTuple(getElement)
      }
      else {
        remove(cache.last)
        insertTuple(getElement)
      }
    }
  }

  def getElementPolicyLSL(getElement: Tuple2[Key, Value]) = {
    if(!cache.contains(getElement)) {
      if(cacheCurrentSize < cacheCapacity) {
        insertTuple(getElement)
      }
      else {
        var delElement = cache(0)
        var minVal = 1000
        for(i <- cache.indices) {
          if(cache(i)._1.asInstanceOf[String].length() < minVal) {
            minVal = cache(i)._1.asInstanceOf[String].length()
            delElement = cache(i)
          }
        }
        remove(delElement)
        insertTuple(getElement)
      }
    }
    else {
      remove(getElement)
      insertTuple(getElement)
    }
  }

  def getElementPolicyLFU(getElement: Tuple2[Key, Value]) = {
    if(!cache.contains(getElement)) {
      if(cacheCurrentSize < cacheCapacity) {
        insertTuple(getElement)
      }
      else {
        var delElement = cache(0)
        var minVal = 1000
        for(index <- cache.indices) {
          if(allElements.count(_ == cache(index)) < minVal) {
            minVal = allElements.count(_ == cache(index))
            delElement = cache(index)
          }
        }
        remove(delElement)
        insertTuple(getElement)
      }
    }
    else {
      remove(getElement)
      insertTuple(getElement)
    }
  }

  def getElementPolicyMFU(getElement: Tuple2[Key, Value]) = {
    if(!cache.contains(getElement)) {
      if(cacheCurrentSize < cacheCapacity) {
        insertTuple(getElement)
      }
      else {
        var delElement = cache(0)
        var maxVal = 0
        for (index <- cache.indices) {
          if (allElements.count(_ == cache(index)) > maxVal) {
            maxVal = allElements.count(_ == cache(index))
            delElement = cache(index)
          }
        }
        remove(delElement)
        insertTuple(getElement)
      }
    }
    else {
      remove(getElement)
      insertTuple(getElement)
    }
  }

  def getElementPolicyFIFO(getElement: Tuple2[Key, Value]) = {
    if(cache.contains(getElement)) {
      remove(getElement)
      insertTuple(getElement)
    }
    else {
      if(cacheCurrentSize < cacheCapacity) {
        insertTuple(getElement)
      }
      else {
        var delElement = cache(0)
        for(index <- allElements.indices) {
          if(cache.contains(allElements(index))) {
            if(delElement == cache(0)) {
              delElement = allElements(index)
            }
          }
        }
        remove(delElement)
        insertTuple(getElement)
      }
    }
  }
}
