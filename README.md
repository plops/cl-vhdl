# cl-vhdl

## Introduction

The purpose of this code is to provide a domain specific language in
Common Lisp that converts s-expressions into valid VHDL strings.

The only exported interface is the function:

### vhdl:emit <s-expression>

## Implementation

The file vhdl-examples.lisp lists a few VHDL code examples and a
proposal for their representation as s-expressions.

The file vhdl-gen.lisp contains the implementation of the function
emit. This function is of the same structure as
cl-cpp-generator:emit-cpp, which I developed earlier. A big case
statement acts upon the first element of each list, calls print macros
or recurses into sub-s-expressions.

The function emit keeps track of the indentation level using the global
variable *level* and the macro with-indent.

Currently, I add s-expressions and their string representation at the
bottom of the file vhdl-gen.lisp. Eventually, this code will go into
its own file and I will use SBCL's code coverage tool to verify that
all the execution paths in vhdl:emit are tested.

## References

MIT 6.004 Lecture Spring 2016

## License

This code is released under GPL.