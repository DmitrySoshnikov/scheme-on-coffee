# Pre-parsing of Scheme expression to JS/Coffee array
#
# by Dmitry Soshnikov <dmitry.soshnikov@gmail.com>
# (C) MIT Style License
#

window.parse = (exp) ->

  exp = exp
    .replace(/;.*$/gm, "") # strip comments
    .replace(/^\s+|\s+$/g, "") # and trailing whitespaces

  return exp if isVariable(exp) or exp is ''

  exp = exp
    .replace(/\'\(/g, "(list ") # and replace Lisp's '(1 2 3) with (list 1, 2, 3)
    .replace(/\'([^ ]+)/g, "(quote $1)") # and remove ' to handle 'list
    .replace(/apply\s*(.+)\(list\s*([^)]+)\)/g, "$1 $2") # replace (apply <proc> (list 1 2 3)) with (proc 1 2 3)
    .replace(/\(/g, "[") # replace Lisp's parens...
    .replace(/\)/g, "]") # ... with JS's array squares
    .replace(/\s+/g, ",") # replace spaces in expressions with commas to get real JS arrays
    .replace(/([^,\[\]0-9]+?(?=(,|\])))/g, "'$1'") # quote the names

  JSArray2LispList(eval(exp)) # build the JS array convert it to Lisp's list