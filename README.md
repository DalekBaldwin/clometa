# CLOMeta

[![Build Status](https://travis-ci.org/DalekBaldwin/clometa.svg?branch=master)](https://travis-ci.org/DalekBaldwin/clometa)

This repository contains a non-fully-featured OMeta interpreter based on [Vlad Kozin's implementation in Racket](https://github.com/vkz/ometa-racket), and an almost-complete CLOMeta compiler bootstrapped from that interpreter. This is not an OMeta implementation in Common Lisp. If it were, it would be called `cl-ometa`, but instead it's a non-hyphenated `clometa`. It's still in the feel-out-the-proper-shape-of-the-code-as-I-write-it phase, so what follows is more of an explanation of design choices so far than proper documentation.

## Syntax

Most OMeta implementations seem to copy the syntax of OMeta/JS, requiring grammars to be written in separate files or as strings that are preprocessed (perhaps by the OMeta implementation itself). CLOMeta is embedded as a DSL. This allows for a straightforward mapping onto ContextL: grammars are context layers, and rules are methods specialized in those layers. Using `(next-rule)` within a rule translates straightforwardly into a call to `call-next-layered-method`. Also, there's no need for grammars to have instance variables - the programmer can manage state more flexibly using the surrounding namespace.

Using a Lispy, parenful syntax causes some tension in trying to bridge the worlds of parsing and pattern matching. In Lispy pattern-matching, parentheses visually indicate the a static *structural* pattern you want to match. But in early parsing phases like tokenization, rules mostly need to match against structurally indistinct subsequences. The great virtue of homoiconicity is that syntax mirrors structure, but this comes at a cost. Recall that when learning a Lisp dialect for the first time, you have to slowly develop a feel for where there are implicit `progn`-type constructs, but once you do, you can write and understand very concise code. CLOMeta uses implicit `seq` clauses in three places: surrounding rule bodies, and surrounding the insides of `list` and `?` clauses. You will usually only need to use explicit `seq` clauses to enclose alternatives in an `or` clause.

To better indicate the boundaries between OMeta expressions and plain Lisp expressions, semantic actions are preceded by keywords `:->?` and `:->` (instead of being wrapped in s-expressions with `->?` and `->` as the head):

```lisp
(defgrammar grammar (supergrammar)
  (rule (arg1 arg2)
        ;; CLOMeta code
        (or (bind x (another-rule))
            (bind y (yet-another-rule)))
             ;; Lisp code
        :->? (test-predicate x y)
            ;; more Lisp code
        :-> (semantic-action x y)))
```

## Operations

```lisp
;; negation - does not consume input
(~ exp)

(bind var exp)

;; sequence - returns result of final expression
(seq exp ...)

(or exp ...)

;; Kleene star
(* exp)

;; Kleene plus
(+ exp)

;; zero or one - expressions wrapped in implicit seq clause
(? exp ...)

;; list - expressions wrapped in implicit seq clause
(list exp ...)

;; anything (despite the symbol's typical meaning in pattern matchers, you can still bind its result)
_

;; invoke rule by name
(rule arg ...)

;; apply rule by value - rules are methods, so they can be passed around with #'rule
(apply rule arg ...)

;; invoke rule with same name in supergrammar - applies same args by default
(next-rule)

;; invoke a rule that does not appear in this grammar or its supergrammars
(foreign grammar rule arg ...)

;; semantic action - always succeeds
:-> form

;; semantic predicate - fails if form is nil
:->? form
```

## Semantics

CLOMeta features a more functional/declarative semantics than what is described in section 2.4.2 of Warth's thesis. The official OMeta binding semantics can lead to strange behavior that makes parsers difficult to debug. I feel it's better not to assign anything to a variable in a poorly-written rule than to assign it a value that could be kinda sorta right but in a way you don't notice would break on a different input.  Although parsing patterns are *about* sequences, they need not be written *in terms of* the effects of low-level sequential assignments, and the programmer should not have to pay undue attention to the internal workings of a rule to create a composite pattern on top of it.

This leads to the following major differences from OMeta:

### Alternation
In the `or` pattern, if any variables are assigned within an alternative clause before that alternative is discovered to be a failed match, those assignments will not persist after the `or` pattern returns. Instead, as in [optima](https://github.com/m2ym/optima), any variable appearing in failed or untried alternatives that is not assigned in the successful alternative is bound to `nil`. The semantics of the composite pattern only depends on the fact *that* an alternative failed, not *where* it failed.

### Iteration
Bindings established within repetition patterns (`*`/`+`) are not accessible to the surrounding pattern. This is because a new assignment to a variable within an iteration overwrites the previous value, so only the final iteration's value would be accessible anyway. So typically you should collect all the data you really need using a semantic action and have the repetition pattern return a list of the results of each iteration's final semantic action. (An alternative I'm considering is to collect a list of each variable bound in the subpattern, Racket-style. In this case I'd use a separate operator name, perhaps `accum`/`collect` rather than `bind`, to make the results more obvious.)

### Negation
Bindings established within `~` patterns are not accessible to the surrounding pattern. The rationale is similar to that for alternation; those bindings can only be referenced later if the pattern *doesn't* match, so what would you use them for anyway?

There are other potential changes to the core semantics that might be nice, but they aren't totally necessary because you can use...

### Macros

CLOMeta will expand macros that appear in operator position. CLOMeta follows Core-OMeta semantics for lists, returning the original list object that matched. If you instead want a list of the results of all the subclauses, you can do something like this:

```lisp
(defmacro list<< (&rest clauses)
  (let ((gensyms (loop for clause in clauses collect (gensym))))
    `(seq (list
           ,@(loop for clause in clauses
                for sym in gensyms
                collect `(bind ,sym ,clause)))
          :-> (list ,@gensyms))))
```

Note that this requires an explicit invocation of the macroexpansion facility in a nonstandard position, so take extra care to make sure the macro is available before the `defgrammar` form is processed.

## To Do:

CLOMeta does not yet allow pattern matching on rule parameters. At first glance this seemed to be a good use case for Christophe Rhodes' [generalizers](http://www.european-lisp-symposium.org/rhodes.pdf), but since it looks like OMeta rule arguments must be reassigned the result of the rule application within the body like any other binding, this wouldn't lead to improved modularity or conciseness.
