package machines

import regex._
import dfa._

/** Convert a regular language to a string that uses standard symbols for
  * describing regular languages.
  *
  * @param lang
  *   a regular language
  * @return
  *   a string that describes the regular language
  */
def regexToString(lang: RegularLanguage): String = lang match {
  case Empty          => "∅"
  case Epsilon        => "ε"
  case Character(c)   => c.toString
  case Union(l1, l2)  => s"(${regexToString(l1)} ∪ ${regexToString(l2)})"
  case Concat(l1, l2) => s"(${regexToString(l1)}${regexToString(l2)})"
  case Star(l)        => s"(${regexToString(l)}*)"
}

/** Given a regular language and an alphabet of valid input characters, convert
  * the language to an equivalent DFA.
  *
  * @param r
  *   a regular language
  * @param alphabet
  *   a set of valid input characters
  * @return
  *   a DFA that accepts the regular language.
  */
def regexToDFA(r: RegularLanguage, alphabet: Set[Char]) = {
  // A simplified version of the regular language
  val start = reduce(r)

  // Compute all the derivatives, starting with the initial state,
  // and reduce each derivative to a simplified version
  val derivs = allDerivatives(alphabet, start).map(reduce).toSet

  // Create states for each derivative
  val states = derivs.map(langToState)

  // Create transitions among all the states:
  // For each state, for each character in the alphabet,
  // create a transition to the state that corresponds to the derivative
  // of the current state with respect to the character
  val transitions: Set[Transition] =
    for
      c <- alphabet
      lang <- derivs
    yield Transition(
      langToState(lang),
      langToState(reduce(derivative(lang)(c))),
      c
    )

  // Create the DFA
  DFA(
    states = states,
    transitions = transitions,
    start = langToState(start),
    accept = derivs.filter(nullable).map(langToState)
  )
}

////////////////////////////////////////////////////////////////////////////////
// Helper functions
////////////////////////////////////////////////////////////////////////////////

// Convert a regular language to a DFA state
private def langToState(r: RegularLanguage): State = State(regexToString(r))

// Repeatedly simplify a regular language until it stops changing
private def reduce(lang: RegularLanguage): RegularLanguage = {
  var previous = lang
  var next = simplify(previous)
  while (previous != next) {
    previous = next
    next = simplify(previous)
  }
  next
}

// Compute all the derivatives of a regular language with respect to all the
// characters in the alphabet.
private def allDerivatives(
    alphabet: Set[Char],
    r: RegularLanguage
): List[RegularLanguage] = {
  var worklist: List[RegularLanguage] = List(r)
  var results: List[RegularLanguage] = List()

  // Use a worklist algorithm to take the transitive closure of the derivatives (w.r.t. each
  // character in the alphabet), starting at the initial state
  while (!worklist.isEmpty) {
    val first = worklist.head
    val rest = worklist.tail

    if (results.contains(first)) then {
      worklist = rest // We've already seen this item, so skip it
    } else {
      // Add all the derivatives to the worklist
      val derivatives = alphabet.map(lang => simplify(derivative(first)(lang)))
      worklist ++= derivatives

      // Move the item from the worklist to the results
      results = first +: results
    }
  }
  results
}
