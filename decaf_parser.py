# Ryan Chen 
# ryachen
# 113200236

from decaf_lexer import tokens
import sys
# define grammar rules
# starting rule -- program
def p_program(p):
    '''
    program : class_decl program
            | empty
    '''

# empty production
def p_empty(p):
    'empty :'
    pass

# class declarations
def p_class_declaration(p):
    '''
    class_decl : CLASS ID optionalExtendsId L_CURLY_BRACE class_body_decl_plus R_CURLY_BRACE
    '''

def p_optionalExtendsId(p):
    '''
    optionalExtendsId : EXTENDS ID
                | empty
    '''

def p_classBodyDeclPlus(p):
    '''
    class_body_decl_plus : class_body_decl
                        | class_body_decl class_body_decl_plus
    '''

def p_class_body_decl(p):
    '''
    class_body_decl : field_decl
                    | method_decl
                    | constructor_decl
    '''

# fields
def p_field_decl(p):
    '''
    field_decl : modifier var_decl
    '''

def p_modifier(p):
    '''
    modifier : public_private_zero_or_one static_zero_or_one
    '''

def p_public_private_zero_or_one(p):
    '''
    public_private_zero_or_one : PUBLIC
                                | PRIVATE
                                | empty
    '''

def p_static_zero_or_one(p):
    '''
    static_zero_or_one : STATIC
                        | empty
    '''

def p_var_decl(p):
    '''
    var_decl : type variables SEMI_COLON
    '''

def p_type(p):
    '''
    type : INT
        | FLOAT
        | BOOLEAN
        | ID
    '''

def p_variables(p):
    '''
        variables : variable additional_variables
    '''

def p_additional_variables(p):
    '''
    additional_variables : COMMA variable additional_variables
                        | empty
    '''

def p_variable(p):
    '''
    variable : ID
    '''

def p_method_decl(p):
    '''
    method_decl : modifier type ID  L_PAREN zero_or_one_formals R_PAREN block
                |   modifier VOID ID  L_PAREN zero_or_one_formals R_PAREN block
    
    '''

# def p_types(p):
#     '''
#     types : VOID
#         | type
#     '''
def p_zero_or_one_formals(p):
    '''
    zero_or_one_formals : formals
                        | empty
    '''

def p_constructor_decl(p):
    '''
    constructor_decl : modifier ID L_PAREN zero_or_one_formals R_PAREN block
    '''

def p_formals(p):
    '''
    formals : formal_param additional_formal_params
    '''


def p_additional_formal_params(p):
    '''
    additional_formal_params : COMMA formal_param additional_formal_params
                            | empty
    '''

def p_formal_param(p):
    '''
    formal_param : type variable
    '''

# decaf statements

def p_block(p):
    '''
    block : L_CURLY_BRACE stmt_star R_CURLY_BRACE
    '''

def p_stmt_star(p):
    '''
    stmt_star : stmt stmt_star
                | empty
    '''

def p_stmt(p):
    '''
    stmt : IF L_PAREN expr R_PAREN stmt zero_or_one_else_stmt
        | WHILE L_PAREN expr R_PAREN stmt
        | FOR L_PAREN zero_or_one_stmt_expr SEMI_COLON zero_or_one_expr SEMI_COLON zero_or_one_stmt_expr R_PAREN stmt
        | RETURN zero_or_one_expr SEMI_COLON
        | stmt_expr SEMI_COLON
        | BREAK SEMI_COLON
        | CONTINUE SEMI_COLON
        | block
        | var_decl
        | SEMI_COLON
    '''

def p_zero_or_one_else_stmt(p):
    '''
    zero_or_one_else_stmt : ELSE stmt
                            | empty
    '''

def p_zero_or_one_stmt_expr(p):
    '''
    zero_or_one_stmt_expr : stmt_expr
                            | empty
    '''

def p_zero_or_one_expr(p):
    '''
    zero_or_one_expr : expr
                    | empty
    '''

# Expressions

def p_literal(p):
    '''
    literal : INT_CONST
            | FLOAT_CONST
            | STRING_CONST
            | NULL
            | TRUE
            | FALSE
    '''

def p_primary(p):
    '''
    primary : literal
            | THIS
            | SUPER
            | L_PAREN expr R_PAREN
            | NEW ID L_PAREN zero_or_one_arguments R_PAREN
            | lhs
            | method_invocation
    '''

def p_zero_or_one_arguments(p):
    '''
    zero_or_one_arguments : arguments
                        | empty
    '''

def p_arguments(p):
    '''
    arguments : expr additional_expr
    '''

def p_additional_expr(p):
    '''
    additional_expr : COMMA expr additional_expr
                    | empty
    '''

def p_lhs(p):
    '''
    lhs : field_access
    '''

def p_field_access(p):
    '''
    field_access : primary DOT ID
                | ID
    '''

def p_method_invocation(p):
    '''
    method_invocation : field_access L_PAREN zero_or_one_arguments R_PAREN
    '''

def p_expr(p):
    '''
    expr : primary
        | assign
        | expr arith_op expr
        | expr bool_op expr
        | unary_op expr
    '''

def p_assign(p):
    '''
    assign : lhs ASSIGNMENT_OP expr
            | lhs INCR_OP
            | INCR_OP lhs
            | lhs DECR_OP
            | DECR_OP lhs
    '''

def p_arith_op(p):
    '''
    arith_op : PLUS
            | MINUS
            | TIMES
            | DIVIDE
    '''

def p_bool_op(p):
    '''
    bool_op : LOGICAL_AND_OP
            | LOGICAL_OR_OP
            | EQUALITY_OP
            | DISEQUALITY_OP
            | L_THAN_OP
            | G_THAN_OP
            | L_THAN_EQUAL_TO_OP
            | G_THAN_EQUAL_TO_OP
    '''

def p_unary_op(p):
    '''
    unary_op : PLUS
            | MINUS
            | NEG_OP
    '''

def p_stmt_expr(p):
    '''
    stmt_expr : assign
            | method_invocation
    '''



# Error rule for syntax errors
def p_error(p):
    print("Syntax error in input!")
    print("Illegal Character '%s', at %d , %d" % (p.value[0], p.lineno, p.lexpos))
    print()
    sys.exit()
