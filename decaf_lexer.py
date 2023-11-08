# Ryan Chen 
# ryachen
# 113200236

import sys

reserved = {
    "boolean" : "BOOLEAN",
    "break" : "BREAK",
    "continue" : "CONTINUE",
    "class" : "CLASS",
    # "do" : "DO",
    "else" : "ELSE",
    "extends" : "EXTENDS",
    "false" : "FALSE",
    "float" : "FLOAT",
    "for" : "FOR",
    "if" : "IF",
    "int" : "INT",
    "new" : "NEW",
    "null" : "NULL",
    "private" : "PRIVATE",
    "public" : "PUBLIC",
    "return" : "RETURN",
    "static" : "STATIC",
    "super" : "SUPER",
    "this" : "THIS",
    "true" : "TRUE",
    "void" : "VOID",
    "while" : "WHILE"
}


# token tuple -> to modify
tokens = ["ID", # identifier
          "INT_CONST", #constants
          "FLOAT_CONST",
          "STRING_CONST",

          # syntax stuff and operators
          "L_CURLY_BRACE", # {
          "R_CURLY_BRACE", # }
          "L_PAREN", # (
          "R_PAREN", # )
          "SEMI_COLON", # ;
          "ASSIGNMENT_OP", # =
          "INCR_OP", # X++
          "DECR_OP", # X--
          "PLUS", # + for addition, unary plus
          "MINUS", # - for subtraction, unary minus
          "TIMES", # * for multiplication
          "DIVIDE", # / for division
          "EQUALITY_OP", # == equality check
          "DISEQUALITY_OP", # != inequality
          "L_THAN_OP", # < less than
          "G_THAN_OP", # > greater than
          "G_THAN_EQUAL_TO_OP", # >=
          "L_THAN_EQUAL_TO_OP", # <=
          "NEG_OP", # ! negation
          "LOGICAL_AND_OP", # && AND
          "LOGICAL_OR_OP", # || operator

          "COMMA",
          "DOT"
          ]

tokens = tokens + list(reserved.values())

# regular expression rules for simple tokens (not sure if this is right)

# longer expressions come first
t_STRING_CONST = r"\".*\""
t_L_CURLY_BRACE = r"{"
t_R_CURLY_BRACE = r"}"
t_L_PAREN = r"\("
t_R_PAREN = r"\)"
t_SEMI_COLON = r";"
t_INCR_OP = r"\+\+"
t_DECR_OP = r"--"
t_G_THAN_EQUAL_TO_OP = r">="
t_L_THAN_EQUAL_TO_OP = r"<="
t_EQUALITY_OP = r"=="
t_DISEQUALITY_OP = r"!="
t_LOGICAL_AND_OP = r"&&"
t_LOGICAL_OR_OP = r"\|\|"

# shorter expression come later
t_ASSIGNMENT_OP = r"="
t_PLUS = r"\+"
t_MINUS = r"-"
t_TIMES = r"\*"
t_DIVIDE = r"/"
t_L_THAN_OP = r"<"
t_G_THAN_OP = r">"
t_NEG_OP = r"!"
t_COMMA = r","
t_DOT = r"\."

# regular expression rule with action code
def t_IDENTIFIER(t):
    r'[a-zA-Z][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, "ID") # check for reserved keywords
    return t

# float constants have a decimal point and a sequence of 1+ digits on either side
def t_FLOAT_CONST(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

# Integer constants are one or more digits [0-9]
def t_INT_CONST(t):
    r'\d+'
    t.value = ("INT_CONST",int(t.value))
    return t

# define rule to track line no.
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# A string containing ignored characters
t_ignore = " \t\n"

# scanning errors
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    # t.lexer.skip(1)
    sys.exit()

# ignoring comments

# handle multi-line comments

def t_COMMENT_MULTI_LINE(t):
    r"\/\*[\s\S]*?\*\/"
    pass

# handle single-line comments
def t_COMMENT_SINGLE_LINE(t):
    r"\/\/[^\n]*"
    pass
