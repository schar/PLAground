module Instructions where

import Markdown
import Html (..)

instructions : Html
instructions = Markdown.toHtml inst

inst : String
inst =
  """
  # User Interface
  
  * At the prompt, type an expression of FOL and then press Enter.
  * If your expression is well-formed, it will be evaluated at the current
    input stack (empty by default), and the resulting set of outputs will
    appear below.
  * Subsequent queries will be dynamically conjoined to those that have
    already been evaluated.
  * You can change the initial context at any time by placing individuals on 
    the input stack (e.g. [1], [423], etc.). You'll see the effects percolate
    through the discourse history immediately.
  * You can also mute previous queries either to undo them, or to see how the
    rest of the discourse would have played out, had those expressions not
    been uttered. Mute a query by clicking on the x next to its parsetree in
    the expression record.
  * If your query expression is not well-formed or refers to a
    variable/pronoun beyond the reach of the current context, you will see a
    terse error message, either '*' (parse error), 'x?' (where 'x' is a free
    variable in your expr), or 'pk?' (where 'k' is an index greater than the
    size of the current stack).


  # Technical Reference

  ## Domain and Lexicon

  * Formulas are evaluated against a domain with four individuals D :=
    {1,2,3,4}, and an empty assignment function g := λv. Error.
  * There are two predicates, 'e' and 'o', and one relation 'eq':
      + ⟦e⟧ := λx. x ∈ {2,4}
      + ⟦o⟧ := λx. x ∈ {1,3}
      + ⟦eq⟧ := λx.λy. (x,y) ∈ {(1,1), (2,2), (3,3), (4,4)}


  ## Predicate Logic with Anaphora ([Dekker 1994](http://elanguage.net/journals/salt/article/view/4.79))

  * Terms are evaluated wrt an assignment (mostly implicit in the defs below)
    and incoming contexts. For v ∈ VAR, c ∈ CON, k ∈ N:
      + ⟦v⟧[g](#) := λs. g(v)
      + ⟦c⟧ := λs. c, for c = 1, 2, 3, 4
      + ⟦pk⟧ := λs. s_k, where s = [s_n,...,s_k,...,s_1, s_0]
  * Formulas are evaluted at the same parameters. Again the assignments are
    left implicit, except where necessary:
      + ⟦R(t1,...,tn)⟧ := λs. {s | ⟦R⟧(⟦t1⟧s)...(⟦tn⟧s)}
      + ⟦~Φ⟧ := λs. {s | ⟦Φ⟧(s) ≠ ∅}
      + ⟦(Φ & Ψ)⟧ := λs. {s'' | s' ∈ ⟦Φ⟧(s), s'' ∈ ⟦Ψ⟧(s')}
      + ⟦Ex Φ⟧ := λs. {s' • d | d ∈ D, s' ∈ ⟦Φ⟧[x↦d](#)(s)}
  """
