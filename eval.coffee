# Lisp/Scheme evaluator, see: SICP, 4.1
#
# Port to CoffeeScript
# by Dmitry Soshnikov <dmitry.soshnikov@gmail.com>
# (C) MIT Style License
#
# Features:
#   - just a toy and very inefficient; just the version 0.1 ;)
#
# Scheme metacircular evaluator authors: book SICP
# see http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-26.html#%_sec_4.1
#
# All comments in code are *not* my comments. This is the
# text of the chapter 4.1 of the SICP book
#

this.LispMachine =

  ## --------------------------
  ## Eval
  ## --------------------------

  # Eval takes as arguments an expression (exp)
  # and an environment (env). It classifies the expression
  # and directs its evaluation. Eval is structured as a case
  # analysis of the syntactic type of the expression to be
  # evaluated. In order to keep the procedure general, we express
  # the determination of the type of an expression abstractly, making
  # no commitment to any particular representation for the various
  # types of expressions. Each type of expression has a predicate that tests
  # for it and an abstract means for selecting its parts. This *abstract syntax*
  # makes it easy to see how we can change the syntax of the language by using
  # the same evaluator, but with a different collection of syntax procedures.

  eval: (exp, env) ->

    ## --------------------------
    ## Primitive expressions
    ## --------------------------

    # For self-evaluating expressions, such as numbers, eval returns the
    # expression itself.

    # console.log "isSelfEvaluating #{exp}", isSelfEvaluating(exp), typeof exp
    if isSelfEvaluating exp
      return exp

    # Eval must look up variables in the environment to find their values.

    # console.log "isVariable #{exp}", isVariable exp
    if isVariable exp
      return lookupVariableValue exp, env

    ## --------------------------
    ## Special forms
    ## --------------------------

    # For quoted expressions, eval returns the expression that was quoted.

    # console.log "isQuoted #{exp}", isQuoted exp
    if isQuoted exp
      return textOfQuotation exp

    # An assignment to (or a definition of) a variable must recursively
    # call eval to compute the new value to be associated with the variable.
    # The environment must be modified to change (or create) the binding
    # of the variable.

    # console.log "isAssignment #{exp}", isAssignment exp
    if isAssignment exp
      return evalAssignment exp, env

    # console.log "isDefinition #{exp}", isDefinition exp
    if isDefinition exp
      return evalDefinition exp, env

    # An if expression requires special processing of its parts, so as to evaluate
    # the consequent if the predicate is true, and otherwise to evaluate the alternative.

    # console.log "isIf #{exp}", isIf exp
    if isIf exp
      return evalIf exp, env

    # A lambda expression must be transformed into an applicable procedure by packaging
    # together the parameters and body specified by the lambda expression with the
    # environment of the evaluation.

    #console.log "isLambda #{exp}", isLambda exp
    if isLambda exp
      return makeProcedure(
        lambdaParameters(exp),
        lambdaBody(exp),
        env
      )

    # A begin expression requires evaluating its sequence of expressions in the order
    # in which they appear.

    # console.log "isBegin #{exp}", isBegin exp
    if isBegin exp
      return evalSequence(
        beginActions(exp),
        env
      )

    # A case analysis (cond) is transformed into a nest of if expressions and then evaluated.

    # console.log "isCond #{exp}", isCond exp
    if isCond exp
      return LispMachine.eval(
        condIf(exp),
        env
      )

    ## --------------------------
    ## Combinations
    ## --------------------------

    # For a procedure application, eval must recursively evaluate the operator part and the
    # operands of the combination. The resulting procedure and arguments are passed to apply,
    # which handles the actual procedure application.

    #console.log "isApplication #{exp}", isApplication exp
    if isApplication exp
      return LispMachine.apply(
        LispMachine.eval(operator(exp), env),
        listOfValues(operands(exp), env)
      )

    throw "Unknown expression type -- LispMachine.eval: #{exp}"

  ## --------------------------
  ## Apply
  ## --------------------------

  # Apply takes two arguments, a procedure and a list of arguments to which the procedure
  # should be applied. Apply classifies procedures into two kinds: It calls *applyPrimitiveProcedure*
  # to apply primitives; it applies compound procedures by sequentially evaluating the expressions
  # that make up the body of the procedure. The environment for the evaluation of the body of a compound
  # procedure is constructed by extending the base environment carried by the procedure to include a frame
  # that binds the parameters of the procedure to the arguments to which the procedure is to be applied.

  apply: (procedure, arguments) ->

    #console.log "isPrimitiveProcedure", isPrimitiveProcedure procedure
    if isPrimitiveProcedure procedure
      return applyPrimitiveProcedure procedure, arguments

    #console.log "isCompoundProcedure", isCompoundProcedure procedure
    if isCompoundProcedure procedure
      return evalSequence(
        procedureBody(procedure),
        extendEnvironment(
          procedureParameters(procedure),
          arguments,
          procedureEnvironment(procedure)
        )
      )

    throw "Unknown procedure type -- apply: #{procedure}"


##
## Helpers (global functions)
##

extend = (object, module) ->
  for own k of module
    object[k] = module[k]
  object

extend this,

  ## --------------------------
  ## Procedure arguments
  ## --------------------------

  # When eval processes a procedure application, it uses *listOfValues* to produce the list of arguments
  # to which the procedure is to be applied. ListOfValues takes as an argument the operands of the combination.
  # It evaluates each operand and returns a list of the corresponding values

  listOfValues: (exps, env) ->
    return null if noOperands exps
    return cons(
      LispMachine.eval(firstOperand(exps), env),
      listOfValues(restOperands(exps), env)
    )

  ## --------------------------
  ## Conditionals
  ## --------------------------

  # EvalIf evaluates the predicate part of an if expression in the given environment. If the result is true, evalIf
  # evaluates the consequent, otherwise it evaluates the alternative:

  evalIf: (exp, env) ->
    if LispMachine.eval(ifPredicate(exp), env)
      return LispMachine.eval(ifConsequent(exp), env)

    return LispMachine.eval(ifAlternative(exp), env)

  ## --------------------------
  ## Sequences
  ## --------------------------

  # EvalSequence is used by apply to evaluate the sequence of expressions in a procedure body and by eval to evaluate
  # the sequence of expressions in a begin expression. It takes as arguments a sequence of expressions and an environment,
  # and evaluates the expressions in the order in which they occur. The value returned is the value of the final expression.

  evalSequence: (exps, env) ->

    if isLastExp exps
      return LispMachine.eval(firstExp(exps), env)

    LispMachine.eval(firstExp(exps), env)
    evalSequence(restExps(exps), env)

  ## --------------------------
  ## Assignments and definitions
  ## --------------------------

  # The following procedure handles assignments to variables. It calls eval to find the value to be assigned and transmits the
  # variable and the resulting value to setVariableValue to be installed in the designated environment.

  evalAssignment: (exp, env) ->
    setVariableValue(
      assignmentVariable(exp),
      LispMachine.eval(assignmentValue(exp), env),
      env
    )

  # Definitions of variables are handled in a similar manner.

  evalDefinition: (exp, env) ->
    defineVariable(
      definitionVariable(exp),
      LispMachine.eval(definitionValue(exp), env),
      env
    )

## ---------------------------------
## 4.1.2  Representing Expressions
## ---------------------------------

extend this,

  # The only self-evaluating items are numbers and strings:

  isSelfEvaluating: (exp) ->
    return true if isNumber(exp) # or isString(exp)
    false

  # ad-hoc
  isNumber: (exp) ->
    not isNaN(Number(exp))

  # ad-hoc
  isString: (exp) ->
    typeof exp is "string"

  # Variables are represented by symbols:

  isVariable: (exp) ->
    isSymbol exp

  # ad-hoc
  isSymbol: (exp) ->
    /^([a-z-A-Z0-9_$?!+*/=><\-]|>=|<=)+$/.test exp

  # Quotations have the form (quote <text-of-quotation>)

  isQuoted: (exp) ->
    isTaggedList exp, 'quote'

  textOfQuotation: (exp) ->
    cadr exp

  # `Quoted` is defined in terms of the procedure `isTaggedList`, which
  # identifies lists beginning with a designated symbol:

  isTaggedList: (exp, tag) ->
    return false if not isPair exp
    testTypeTag = (x) ->
      eq car(exp), x
    return tag.some testTypeTag if tag instanceof Array
    testTypeTag tag

  # ad-hoc
  isPair: (o) ->
    o instanceof Array and o.length is 2

  cons: (x, y) ->
    [x, y]

  # ad-hoc
  car: (p) ->
    p[0] # (car '(1 2 3 4)) is 1

  # ad-hoc
  cdr: (p) ->
    p[1] # (cdr '(1 2 3 4)) is '(2 3 4)

  # ad-hoc
  cadr: (o) ->
    car(cdr(o)) # (cadr '(1 2 3 4)) is 2

  caadr: (o) ->
    car(car(cdr(o))) # (caadr '(1 (2 5) 3 4)) is 2

  # ad-hoc
  caddr: (o) ->
    car(cdr(cdr(o))) # (caddr '(1 2 3 4)) is 3

  # ad-hoc
  cdadr: (o) ->
    cdr(car(cdr(o))) # (cdadr '(1 (2 5) 3 4)) is (5)

  # ad-hoc
  cddr: (o) ->
    cdr(cdr(o)) # (cddr '(1 2 3 4)) is (3 4)

  # ad-hoc
  cdddr: (o) ->
    cdr(cdr(cdr(o))) # (cdddr '(1 2 3 4 5)) is (4 5)

  # ad-hoc
  cadddr: (o) ->
    car(cdr(cdr(cdr(o)))) # (cdddr '(1 2 3 4 5)) is 4

  # ad-hoc
  eq: (x, y) ->
    x == y

  # ad-hoc
  list: (items...) ->
    items.reduceRight(
      (y, x) -> [x, y],
      null
    )

  # ad-hoc
  # converts JS's array into Lisp's list
  # Example: [1, 2, 3, 4] => [1, [2, [3, [4, null]]]]
  # Rationale: to make cdr mutable
  JSArray2LispList: (array) ->
    array.reduceRight(
      ((y, x) ->
        if x instanceof Array then cons(JSArray2LispList(x), y) else cons(x, y)),
      null
    )

  # ad-hoc
  # convers Lisp's list into JS's array
  # Example: [1, [2, [3, [4, null]]]] => [1, 2, 3, 4]
  # Rationale: use apply of JS functions
  LispList2JSArray: (list) ->
    retVal = []
    while list
      currentCar = car list
      retVal.push(if isPair(currentCar) then LispList2JSArray(currentCar) else currentCar)
      list = cdr list
    retVal

  # Assignments have the form (set! <variable> <value>)

  isAssignment: (exp) ->
    isTaggedList exp, 'set!'

  assignmentVariable: (exp) ->
    cadr exp

  assignmentValue: (exp) ->
    caddr exp

  # Definitions have the form
  #
  # (define <variable> <value>)
  #
  # or the form
  #
  # (define (<variable> <parameter1> ... <parametern>)
  #   <body>)
  #
  # The latter form (standard procedure definition) is syntactic sugar for
  #
  # (define <variable>
  #   (lambda (<parameter1> ... <parametern>)
  #     <body>))
  #
  # The corresponding syntax procedures are the following:

  isDefinition: (exp) ->
    isTaggedList exp, ['define', 'setf']

  definitionVariable: (exp) ->
    return cadr exp if isSymbol cadr exp
    caadr exp

  definitionValue: (exp) ->
    return caddr exp if isSymbol cadr exp
    makeLambda(
      cdadr(exp), # formal parameters
      cddr(exp) # body
    )

  # Lambda expressions are lists that begin with the symbol lambda:

  isLambda: (exp) ->
    isTaggedList exp, 'lambda'

  lambdaParameters: (exp) ->
    cadr exp

  lambdaBody: (exp) ->
    cddr exp

  # We also provide a constructor for lambda expressions, which is
  # used by definition-value, above:

  makeLambda: (parameters, body) ->
    cons('lambda', cons(parameters, body))

  # Conditionals begin with if and have a predicate, a consequent, and an (optional)
  # alternative. If the expression has no alternative part, we provide false as the alternative.

  isIf: (exp) ->
    isTaggedList exp, 'if'

  ifPredicate: (exp) ->
    cadr exp

  ifConsequent: (exp) ->
    caddr exp

  ifAlternative: (exp) ->
    return cadddr(exp) if cdddr(exp) != null
    false

  # We also provide a constructor for if expressions, to be used by cond->if to transform
  # cond expressions into if expressions:

  makeIf: (predicate, consequent, alternative) ->
    list 'if', predicate, consequent, alternative

  # Begin packages a sequence of expressions into a single expression. We include syntax
  # operations on begin expressions to extract the actual sequence from the begin expression,
  # as well as selectors that return the first expression and the rest of the expressions in the sequence

  isBegin: (exp) ->
    isTaggedList exp, 'begin'

  beginActions: (exp) ->
    cdr exp

  isLastExp: (seq) ->
    cdr(seq) is null

  firstExp: (seq) ->
    car seq

  restExps: (seq) ->
    cdr seq

  # We also include a constructor sequenceExp (for use by condIf) that transforms a sequence into
  # a single expression, using begin if necessary:

  sequenceExp: (seq) ->
    return seq if seq is null
    return firstExp seq if isLastExp seq
    makeBegin seq

  makeBegin: (seq) ->
    cons 'begin', seq

  # A procedure application is any compound expression that is not one of the above expression types.
  # The car of the expression is the operator, and the cdr is the list of operands:

  isApplication: (exp) ->
    isPair exp

  operator: (exp) ->
    car exp

  operands: (exp) ->
    cdr exp

  noOperands: (ops) ->
    ops is null

  firstOperand: (ops) ->
    car ops

  restOperands: (ops) ->
    cdr ops

  ## ---------------------------------
  ## Derived expressions
  ## ---------------------------------

  # Some special forms in our language can be defined in terms of expressions involving other special forms,
  # rather than being implemented directly. One example is cond, which can be implemented as a nest of if expressions.
  # For example, we can reduce the problem of evaluating the expression
  #
  #  (cond ((> x 0) x)
  #        ((= x 0) (display 'zero) 0)
  #        (else (- x)))
  #
  # to the problem of evaluating the following expression involving if and begin expressions:
  #
  # (if (> x 0)
  #     x
  #     (if (= x 0)
  #         (begin (display 'zero)
  #                0)
  #         (- x)))
  #
  # Implementing the evaluation of cond in this way simplifies the evaluator because it reduces the number of special
  # forms for which the evaluation process must be explicitly specified.
  #
  # We include syntax procedures that extract the parts of a cond expression, and a procedure cond->if that transforms
  # cond expressions into if expressions. A case analysis begins with cond and has a list of predicate-action clauses.
  # A clause is an else clause if its predicate is the symbol else.

  isCond: (exp) ->
    isTaggedList exp, 'cond'

  condClauses: (exp) ->
    cdr exp

  isCondElseClause: (clause) ->
    eq condPredicate(clause), 'else'

  condPredicate: (clause) ->
    car clause

  condActions: (clause) ->
    cdr clause

  condIf: (exp) ->
    expandClauses condClauses(exp)

  expandClauses: (clauses) ->
    return false if clauses is null # no else clause
    first = car clauses
    rest = cdr clauses
    if isCondElseClause first
      throw "ELSE clause isn't last -- COND->IF: #{clauses}" if rest is not null
      return sequenceExp condActions(first)
    makeIf(
      condPredicate(first),
      sequenceExp(condActions(first)),
      expandClauses(rest)
    )

## ---------------------------------
## 4.1.3  Evaluator Data Structures
## ---------------------------------

# In addition to defining the external syntax of expressions, the evaluator implementation must also define the data
# structures that the evaluator manipulates internally, as part of the execution of a program, such as the representation
# of procedures and environments and the representation of true and false.

extend this,

  ## ---------------------------------
  ## Testing of predicates
  ## ---------------------------------

  # For conditionals, we accept anything to be true that is not the explicit false object.

  isTrue: (x) ->
    not eq x ,false

  isFalse: (x) ->
    eq x, false

  ## ---------------------------------
  ## Representing procedures
  ## ---------------------------------

  # To handle primitives, we assume that we have available the following procedures:
  #
  # * applyPrimitiveProcedure <proc>, <args>
  #   applies the given primitive procedure to the argument values in the list <args> and returns the result of the application.
  #
  # * isPrimitiveProcedure <proc>
  #   tests whether <proc> is a primitive procedure.
  #
  # These mechanisms for handling primitives are further described in section 4.1.4.
  #
  # Compound procedures are constructed from parameters, procedure bodies, and environments using the constructor make-procedure:

  makeProcedure: (parameters, body, env) ->
    #list 'procedure', parameters, body, env

    # because of posible circular reference we
    # store environment of the procedure as separate property
    procedureObject = list 'procedure', parameters, body
    procedureObject.environment = env
    procedureObject

  isCompoundProcedure: (p) ->
    isTaggedList p, 'procedure'

  procedureParameters : (p) ->
    cadr p

  procedureBody: (p) ->
    caddr p

  procedureEnvironment: (p) ->
    #cadddr p

    # since we have circular reference in case of using
    # list structure to store environment in procedure,
    # we instead use separate property for that
    p.environment

  ## ---------------------------------
  ## Operations on Environments
  ## ---------------------------------

  # The evaluator needs operations for manipulating environments. As explained in section 3.2, an environment is a sequence of frames,
  # where each frame is a table of bindings that associate variables with their corresponding values. We use the following operations
  # for manipulating environments:
  #
  #   * (lookup-variable-value <variable> <env>)
  #     returns the value that is bound to the symbol <variable> in the environment <env>, or signals an error if the variable is unbound.
  #
  #   * (extend-environment <variables> <values> <base-env>)
  #     returns a new environment, consisting of a new frame in which the symbols in the list <variables> are bound to the corresponding
  #     elements in the list <values>, where the enclosing environment is the environment <base-env>.
  #
  #   * (define-variable! <variable> <value> <env>)
  #     adds to the first frame in the environment <env> a new binding that associates the variable <variable> with the value <value>.
  #
  #   * (set-variable-value! <variable> <value> <env>)
  #     changes the binding of the variable <variable> in the environment <env> so that the variable is now bound to the value <value>, or
  #     signals an error if the variable is unbound.
  #
  # To implement these operations we represent an environment as a list of frames. The enclosing environment of an environment is the cdr
  # of the list. The empty environment is simply the empty list.
  #
  # ! The method described here is only one of many plausible ways to represent environments. Since we used data abstraction to isolate
  # the rest of the evaluator from the detailed choice of representation, we could change the environment representation if we wanted
  # to. (See exercise 4.11.) In a production-quality Lisp system, the speed of the evaluator's environment operations -- especially
  # that of variable lookup -- has a major impact on the performance of the system. The representation described here, although conceptually
  # simple, is not efficient and would not ordinarily be used in a production system.

  enclosingEnvironment: (env) ->
    cdr env

  firstFrame: (env) ->
    car env

  TheEmptyEnvironment: []

  # Each frame of an environment is represented as a pair of lists: a list of the variables bound in that frame and a list of
  # the associated values.

  makeFrame: (variables, values) ->
    cons variables, values

  frameVariables: (frame) ->
    car frame

  frameValues: (frame) ->
    cdr frame

  addBindingToFrame: (variable, val, frame) ->
    setCar frame, cons(variable, car(frame))
    setCdr frame, cons(val, cdr(frame))

  # ad-hoc
  setCar: (p, v) ->
    p[0] = v

  # ad-hoc
  setCdr: (p, v) ->
    p[1] = v

  # To extend an environment by a new frame that associates variables with values, we make a frame consisting of the list of
  # variables and the list of values, and we adjoin this to the environment. We signal an error if the number of variables
  # does not match the number of values.

  extendEnvironment: (vars, vals, baseEnv) ->
    #console.log "extendEnvironment:", vars, vals, baseEnv
    throw "Count of vars and vals is not the same: #{vars}, #{vals}" if vars.length != vals.length
    cons(makeFrame(vars, vals), baseEnv)

  # To look up a variable in an environment, we scan the list of variables in the first frame. If we find the desired
  # variable, we return the corresponding element in the list of values. If we do not find the variable in the current
  # frame, we search the enclosing environment, and so on. If we reach the empty environment, we signal an ``unbound variable'' error.

  lookupVariableValue: (variable, env) ->

    envLoop = (env) ->
      scan = (vars, vals) ->
        return envLoop enclosingEnvironment(env) if vars is null
        return car vals if car(vars) is variable
        scan cdr(vars), cdr(vals)

      return "Error: Unbound variable: \"#{variable}\"" if eq env, TheEmptyEnvironment
      frame = firstFrame env
      scan frameVariables(frame), frameValues(frame)

    envLoop env

  # To set a variable to a new value in a specified environment, we scan for the variable, just as in lookup-variable-value,
  # and change the corresponding value when we find it.

  setVariableValue: (variable, val, env) ->

    envLoop = (env) ->

      scan = (vars, vals) ->
        return envLoop enclosingEnvironment(env) if vars is null
        return setCar vals, val if eq variable, car(vars)
        scan cdr(vars), cdr(vals)

      throw "Unbound variable -- SET: \"#{variable}\"" if eq env, TheEmptyEnvironment
      frame = firstFrame env
      scan frameVariables(frame), frameValues(frame)

    envLoop env

  # To define a variable, we search the first frame for a binding for the variable, and change the binding if it exists (just
  # as in set-variable-value!). If no such binding exists, we adjoin one to the first frame.

  defineVariable: (variable, val, env) ->
    frame = firstFrame env

    scan = (vars, vals) ->
      if vars is null
        addBindingToFrame variable, val, frame
        return (if val and isCompoundProcedure(val) then "ok" else val)
      if eq variable, car(vars)
        setCar(vals, val)
        return (if val and isCompoundProcedure(val) then "ok" else val)
      scan cdr(vars), cdr(vals)

    scan frameVariables(frame), frameValues(frame)

## ---------------------------------
## 4.1.4 Running the Evaluator as a Program
## ---------------------------------

# Given the evaluator, we have in our hands a description (expressed in CoffeeScript) of the process by which Lisp expressions are evaluated.
# One advantage of expressing the evaluator as a program is that we can run the program. This gives us, running within Lisp, a working
# model of how Lisp itself evaluates expressions. This can serve as a framework for experimenting with evaluation rules, as we shall do
# later in this chapter.
#
# Our evaluator program reduces expressions ultimately to the application of primitive procedures. Therefore, all that we need to run
# the evaluator is to create a mechanism that calls on the underlying Lisp system to model the application of primitive procedures.
#
# There must be a binding for each primitive procedure name, so that when eval evaluates the operator of an application of a primitive,
# it will find an object to pass to apply. We thus set up a global environment that associates unique objects with the names of the
# primitive procedures that can appear in the expressions we will be evaluating. The global environment also includes bindings for the
# symbols true and false, so that they can be used as variables in expressions to be evaluated.

extend this,

  setupEnvironment: ->

    initialEnv = extendEnvironment(
      primitiveProcedureNames,
      primitiveProcedureObjects,
      TheEmptyEnvironment
    )

    defineVariable 'true', true, initialEnv
    defineVariable 'false', false, initialEnv
    defineVariable 'null', null, initialEnv
    defineVariable 'nil', null, initialEnv

    # define ad-hoc map in the global environment

    execute("
      (define (map proc items)
        (if (null? items)
            nil
            (cons (proc (car items))
                  (map proc (cdr items)))))
    ", initialEnv)

    initialEnv

# It does not matter how we represent the primitive procedure objects, so long as apply can identify and apply them by using
# the procedures primitive-procedure? and apply-primitive-procedure. We have chosen to represent a primitive procedure as a list
# beginning with the symbol primitive and containing a procedure in the underlying Lisp that implements that primitive.


# ad-hoc mapping on Coffee level
this.map = (proc, items) ->
  return null if items is null
  cons(proc(car(items)), map(proc, cdr(items)))

extend this,

  isPrimitiveProcedure: (proc) ->
    isTaggedList proc, 'primitive'

  primitiveImplementation: (proc) ->
    cadr proc

  primitiveProcedures: list(

    # lists
    list('car',      car),
    list('cdr',      cdr),
    list('cons',     cons),
    list('list',     list),
    list('list-ref', (l, i) -> l[i]),
    list('length',   (items) -> items.length),
    list('append',   (x, y) -> JSArray2LispList(x.concat(y))),
    list('reverse',  (x) -> JSArray2LispList(x.reverse())),

    # predicates
    list('true?',  isTrue),
    list('false?', isFalse),
    list('pair?',  isPair),
    list('null?',  (x) -> x is null),

    # relational
    list('=',      eq),
    list('eq',     eq),
    list('>',      (x, y) -> x > y),
    list('>=',     (x, y) -> x >= y),
    list('<',      (x, y) -> x < y),
    list('<=',     (x, y) -> x <= y),

    # some math
    list('+',      (args...) -> args.reduce (a, b) -> a + b),
    list('*',      (args...) -> args.reduce (a, b) -> a * b),

    list('min',    (args...) -> Math.min.apply Math, args),
    list('max',    (args...) -> Math.max.apply Math, args),
    list('abs',    (x) -> if x > 0 then x else -x),

    list('-', (args...) ->
      return -args[0] if args.length is 1
      args.reduce (a, b) -> a - b
    ),

    list('/', (args...) ->
      return 1/args[0] if args.length is 1
      args.reduce (a, b) -> a / b
    )
  )

this.primitiveProcedureNames = map car, primitiveProcedures

this.primitiveProcedureObjects = map ((proc) -> list 'primitive', cadr(proc)), primitiveProcedures

extend this,

  # To apply a primitive procedure, we simply apply the implementation procedure to the arguments, using the
  # underlying Lisp system:

  applyPrimitiveProcedure: (proc, args) ->
    applyProc = primitiveImplementation(proc)

    if applyProc is car or applyProc is cdr
      applyArgs = args
    else if applyProc is cons
      applyArgs = cons(car(args), cadr(args))
    else
      applyArgs = LispList2JSArray(args)

    primitiveImplementation(proc) applyArgs...

  # For convenience in running the metacircular evaluator, we provide a driver loop that models the read-eval-print loop of the
  # underlying Lisp system. It prints a prompt, reads an input expression, evaluates this expression in the global environment,
  # and prints the result. We precede each printed result by an output prompt so as to distinguish the value of the expression
  # from other output that may be printed.

  inputPrompt: ";;; Coffee-Lisp-Eval input:"
  outputPrompt: ";;; Coffee-Lisp-Eval value:"

  driverLoop: (input) ->
    return 'no input' if not input
    # promptForInput inputPrompt
    output = LispMachine.eval(parse(input), TheGlobalEnvironment)
    # announceOutput outputPrompt
    userPrint output
    output

  execute: (input, env) ->
    return 'no input' if not input
    output = LispMachine.eval(parse(input), env or TheGlobalEnvironment)
    if isPair(output)
      return if isPair(cdr(output)) then "(#{LispList2JSArray(output).join(' ')})" else "(#{output.join(' . ')})"
    return "&lt;#procedure \"#{input}\"&gt;" if output and car(output) is "procedure" and isVariable(input)
    output

  promptForInput: (string) ->
    console.log string

  announceOutput: (string) ->
    console.log string

  # We use a special printing procedure, user-print, to avoid printing the environment part of a compound procedure, which may
  # be a very long list (or may even contain cycles).

  userPrint: (object) ->
    if isCompoundProcedure object
      return console.log list('compound-procedure', procedureParameters(object), procedureBody(object), '<procedure-env>')
    console.log object

# Now all we need to do to run the evaluator is to initialize the global environment and start the driver loop.
# Here is a sample interaction:

this.TheGlobalEnvironment = setupEnvironment()

# debug aliases
this.global = TheGlobalEnvironment
this.empty = TheEmptyEnvironment
this.G = TheGlobalEnvironment
this.E = TheEmptyEnvironment

