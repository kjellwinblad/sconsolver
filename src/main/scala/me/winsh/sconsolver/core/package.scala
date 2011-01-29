package me.winsh.sconsolver

package object core {

  private var varIdCounter = 0;

  def getNextVarIdAndIncreaseCounter() = {
    synchronized {
    	
      varIdCounter = varIdCounter + 1

      varIdCounter
    
    }
  }
  

}