package me.winsh.scons

package object core {

	private var varIdCounter = 0;
	
	def getNextVarIdAndIncreaseCounter() = {
		
		varIdCounter = varIdCounter + 1
		
		varIdCounter
		
	}
	
}