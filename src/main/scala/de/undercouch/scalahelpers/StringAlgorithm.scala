// Scala Helpers
// Copyright (c) 2010 Michel Kraemer
//
// This file is released under the terms of the MIT License.
// It is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE. See the MIT License for more details.
//
// You should have received a copy of the MIT License along with
// this file; if not, goto http://www.michel-kraemer.de/en/mit-license

package de.undercouch.scalahelpers

import scala.language.implicitConversions

/**
 * String algorithms that add functionality missing in Java's
 * {@link String} and Scala's {@link scala.collection.immutable.StringLike}.
 * @author Michel Kraemer
 */
final class StringAlgorithm(private val original: String) {
  import StringAlgorithm._
  
  /**
   * Removes characters from the beginning of this string.
   * The given predicate is used to determine if a character
   * should be removed.
   * @param p the predicate. Returns true if the passed character
   * should be removed, returns false if trimming should stop.
   * @return the shortened string
   */
  def trimLeftIf(p: Char => Boolean): String = {
    var s = 0
    while (s < original.length && p(original(s))) s += 1
    if (s == 0)
      original
    else
      original.substring(s)
  }
  
  /**
   * Removes all whitespace characters from the beginning of this string. 
   * @return the shortened string
   */
  def trimLeft() = trimLeftIf(Character.isWhitespace)
  
  /**
   * Removes characters from the end of this string.
   * The given predicate is used to determine if a character
   * should be removed.
   * @param p the predicate. Returns true if the passed character
   * should be removed, returns false if trimming should stop.
   * @return the shortened string
   */
  def trimRightIf(p: Char => Boolean): String = {
    var s = original.length - 1
    while (s >= 0 && p(original(s))) s -= 1
    if (s < 0)
      ""
    else
      original.substring(0, s + 1)
  }
  
  /**
   * Removes all whitespace characters from the end of this string.
   * @return the shortened string
   */
  def trimRight() = trimRightIf(Character.isWhitespace)
  
  /**
   * Removes characters from the beginning and from the end of
   * this string. The given predicate is used to determine
   * if a character should be removed.
   * @param p the predicate. Returns true if a character should
   * be removed.
   * @return the shortened string
   */
  def trimIf(p: Char => Boolean): String = trimLeftIf(p).trimRightIf(p)
  
  /**
   * Splits this string at a certain position when the given predicate
   * returns true for the character at that position. 
   * @param p the predicate. Returns true if the string should be split,
   * false otherwise.
   * @param removeEmpty true if empty parts should be removed from the result
   * @return the separate parts of this string, not including the characters at
   * which the string was split.
   */
  def splitIf(p: Char => Boolean, removeEmpty: Boolean = false): Array[String] = {
    var s = 0
    var e = 0
    var result = List[String]()
    while (e < original.length) {
      if (p(original(e))) {
        result ++= List(original.substring(s, e))
        s = e + 1
      }
      e += 1
    }
    result ++= List(original.substring(s, e))
    if (removeEmpty) result.filter(_.nonEmpty).toArray else result.toArray
  }
  
  /**
   * Checks if this string starts with a given string. Ignores case.
   * @param s the prefix to check for
   * @param offset the offset in this string where the comparison should start 
   * @return true if this string starts with the other one at the given
   * offset, false otherwise
   */
  def startsWithIgnoreCase(s: String, offset: Int = 0) =
    original.regionMatches(true, offset, s, 0, s.length)
  
  /**
   * Checks if this string ends with a given string. Ignores case.
   * @param s the suffix to check for
   * @return true if this string ends with the other one, false otherwise
   */
  def endsWithIgnoreCase(s: String) =
    original.regionMatches(true, original.length - s.length, s, 0, s.length)
}

/**
 * Wraps around a predicate. If this class encounters a double quote
 * character it returns false until it finds the next one. Otherwise
 * it returns the result of the call to the wrapped predicate.
 * @param p the predicate to wrap around
 * @param default the default value to return when the character
 * currently being investigated is a double quote
 * @author Michel Kraemer
 */
class PredicateWithQuotes(private val p: Char => Boolean, default: Boolean = false) extends ((Char) => Boolean) {
  /**
   * True if the parser is currently between quotes
   */
  private var between = false
  
  /**
   * Applies this predicate to a given character
   * @param c the character
   * @return <code>default</code> if c is a double quote character
   * and false if it is between two double quote characters in the
   * string. Otherwise it returns the result of the wrapped predicate.
   */
  def apply(c: Char): Boolean = {
    if (c == '"') {
      between = !between
      default
    } else if (!between) {
      p(c)
    } else {
      false
    }
  }
  
  /**
   * Used for the implicit conversion in {@link StringAlgorithm} so
   * one can call <code>p withQuotes</code> to wrap the predicate <code>p</code>.
   * @return
   */
  def withQuotes = this
  
  /**
   * Used for the implicit conversion in {@link StringAlgorithm} so
   * one can call <code>p withStrippedQuotes</code> to wrap the predicate <code>p</code>.
   * @return
   */
  def withStrippedQuotes = new PredicateWithQuotes(p, true)
}

/**
 * Implicit conversions that pimp {@link String}
 * @author Michel Kraemer
 */
object StringAlgorithm {
  implicit def toStringAlgorithm(s: String): StringAlgorithm = new StringAlgorithm(s)
  implicit def fromStringAlgorithm(s: StringAlgorithm): String = s.original
  implicit def toPredicateWithQuotes(p: Char => Boolean): PredicateWithQuotes = new PredicateWithQuotes(p)
}
