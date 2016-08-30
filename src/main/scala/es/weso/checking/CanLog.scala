package es.weso.checking

import simulacrum._

@typeclass trait CanLog[A] {
  def log(x: String): A
}
