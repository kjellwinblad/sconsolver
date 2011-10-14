package me.winsh.sconsolver

package object core {

  private var varIdCounter:Int = 0;

  def getNextVarIdAndIncreaseCounter() = {
    synchronized {

      varIdCounter = varIdCounter + 1
      if(varIdCounter == Int.MaxValue)
        throw new RuntimeException("The counter excited max value")
      varIdCounter

    }
  }

}