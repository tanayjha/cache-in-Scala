package group


class Test(var CacheCapacity: Int, var cacheCurrentSize: Int) {
  var cache: List[String] = List()
  var allElements: List[String] = List()

  def insertInCache(newElement: String): String = {
    allElements = allElements :+ newElement
    if(cacheCurrentSize < CacheCapacity) {
      cacheCurrentSize += 1
      cache = newElement :: cache
      return "Element added"
    }
    else {
      return "Cache is full"
    }
  }

  def removeFromCache(delElement: String) = {
    if(cache.contains(delElement)) {
      val tempCache = cache.filter(_ != delElement)
      cache = tempCache
      cacheCurrentSize -= 1
    }
    else {
      println("The element is not present in the cache")
    }
  }

  def getElementPolicyLRU(getElement: String): List[String] = {
    if(cache.contains(getElement)) {
      removeFromCache(getElement)
      insertInCache(getElement)
    }
    else {
      if(cacheCurrentSize < CacheCapacity) {
        insertInCache(getElement)
      }
      else {
        removeFromCache(cache.last)
        insertInCache(getElement)
      }
    }
    return cache
  }

  def getElementPolicyLSL(getElement: String): List[String] = {
    if(!cache.contains(getElement)) {
      if(cacheCurrentSize < CacheCapacity) {
        insertInCache(getElement)
      }
      else {
        var delElement = ""
        var minVal = 1000
        for(i <- cache.indices) {
          if(cache(i).length() < minVal) {
            minVal = cache(i).length()
            delElement = cache(i)
          }
        }
        removeFromCache(delElement)
        insertInCache(getElement)
      }
    }
    else {
      removeFromCache(getElement)
      insertInCache(getElement)
    }
    return cache
  }

  def getElementPolicyLFU(getElement: String): List[String] = {
    if(!cache.contains(getElement)) {
      if(cacheCurrentSize < CacheCapacity) {
        insertInCache(getElement)
      }
      else {
        var delElement = ""
        var minVal = 1000
        for(i <- cache.indices) {
          if(allElements.count(_ == cache(i)) < minVal) {
            minVal = allElements.count(_ == cache(i))
            delElement = cache(i)
          }
        }
        removeFromCache(delElement)
        insertInCache(getElement)
      }
      return cache
    }
    else {
      removeFromCache(getElement)
      insertInCache(getElement)
    }
    return cache
  }

  def getElementPolicyMFU(getElement: String): List[String] = {
    if(!cache.contains(getElement)) {
      if(cacheCurrentSize < CacheCapacity) {
        insertInCache(getElement)
      }
      else {
        var delElement = ""
        var maxVal = 0
        for (i <- cache.indices) {
          if (allElements.count(_ == cache(i)) > maxVal) {
            maxVal = allElements.count(_ == cache(i))
            delElement = cache(i)
          }
        }
        removeFromCache(delElement)
        insertInCache(getElement)
      }
    }
    else {
      removeFromCache(getElement)
      insertInCache(getElement)
    }
    return cache
  }

  def getElementPolicyFIFO(getElement: String): List[String] = {
    if(cache.contains(getElement)) {
      removeFromCache(getElement)
      insertInCache(getElement)
    }
    else {
      if(cacheCurrentSize < CacheCapacity) {
        insertInCache(getElement)
      }
      else {
        var delElement = ""
        for(i <- allElements.indices) {
          if(cache.contains(allElements(i))) {
            if(delElement == "") {
              delElement = allElements(i)
            }
          }
        }
        removeFromCache(delElement)
        insertInCache(getElement)
      }
    }
    return cache
  }
}
