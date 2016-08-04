validating - Validating Combinators Library
=========================
[![Build Status](https://travis-ci.org/labra/validating.svg?branch=master)](https://travis-ci.org/labra/validating)

A simple library for validating things

This library is inspired by parser combinators, but instead of combining parsers, it combines validations and constraints 

It defines validated values and constraints which are functions from values and contexts that return either validated values or violation errors

Some libraries that use SRDF are:

* [shaclex](http://labra.github.io/shaclex/)
* [ShExcala](http://labra.github.io/ShExcala/)

See ScalaDoc:

* [ScalaDoc](http://labra.github.io/validating/latest/api/)
* [WebPage](http://labra.github.io/validating/)

# Related links
* [Unified Data Validation Library](https://github.com/jto/validation) is a unification of 
   [Play's Form Validation API](https://www.playframework.com/documentation/2.3.x/ScalaForms) and
   [Json validation API](https://www.playframework.com/documentation/2.3.x/ScalaJsonCombinators). 
   The [Documentation](http://jto.github.io/validation/docs/book/) is very nice.

# Author

Jose Emilio Labra Gayo
