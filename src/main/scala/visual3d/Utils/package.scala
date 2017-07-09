package visual3d

/**
  * Utils package object. Utils has an useful method that can be applied in many situation.
  */
package object Utils {

  /**
    * Clamp limits a value inside its borders.
    * Source: https://plus.cs.hut.fi/studio_2/2017/k17/osa01/
    */
  def clamp[T](value: T, min: T, max: T)(implicit f: (T => Ordered[T])): T = {
    if(value < min) min
    else if (value > max) max
    else value
  }
}
