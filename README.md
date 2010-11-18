
SConSolver - Scala Constraint Solver
====================================

SConSolver is in an early development stage. It is not recommended for any practical use yet.

TODO Before version 0.1
-----------------------

Version 0.1 of SConSolver shall contain the following when it is released:

* Search with possibility to specify search properties to search for the following:
  * Best solution **DONE**
  * All solutions **DONE**
  * First solution **DONE**
  * N solutions **50% DONE**

* Primitive propagators:
  * Equals (x == y) **DONE**
  * NotEquals (x != y) **DONE**
  * LessThanOrEqual (x <= y) **DONE**
  * LessThan (x <= y) **DONE**
  * IsEquals ((x == y) == z)
  * IsNotEquals ((x != y) == z)
  * IsLessThanOrEqual ((x <= y) == z)
  * IsLessThan ((x <= y) == z)
  * Add (x + y == z) **DONE**
  * Sub (x - y == z) **DONE**
  * Mult (x - y == z) **DONE**
  * Div (x / y == z)
  * Modulo (x % y == z)

* Logical propagators (Parameters are variables that can be 0 or 1):
  * Or (x or y == z)
  * And (x and y == z)
  * Not (!x == y)


* Global propagators:
  * Distinct (Takes a list of variables. Guarantees that all variables in the list have unique values.) **DONE**
  * Sum (Takes a list of variables and a result variable. Guarantees that the sum of the variables in the list is equal to the result)
  * Linear (Takes a list of vars List(v1,v2,...,vn) of size n and a list of constants with double values List(c1,c2,...,cn) of size n. Guarantees that the following is true v1*c1+v2*c2+...+vn*cn==0)


* A framework for extending the system with new propagators search methods etc **90% DONE**

* A Domain Specific Language for Constraint Programming **50% DONE**

* Examples:
  * A sudoku solver **70% DONE**
  * Solver for the SEND + MORE + MONEY problem

* Some documentation for the most basic classes

* A short user guide that describes how to get started