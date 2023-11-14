# Ryan Chen 
# ryachen
# 113200236

from decaf_lexer import tokens
from decaf_ast import *
import sys
# define grammar rules
# starting rule -- program
def p_program(p):
    '''
    program : class_decl program
            | empty
    '''
    if len(p) == 3:
        p[2].class_records[p[1].class_name] = p[1]
        p[0] = p[2]
    else: # empty case -> create Class Table
        p[0] = ClassTable()

# empty production
def p_empty(p):
    'empty :'
    pass

# class declarations
def p_class_declaration(p):
    '''
    class_decl : CLASS ID optionalExtendsId L_CURLY_BRACE class_body_decl_plus R_CURLY_BRACE
    '''
    class_decl_res = ClassRecord()
    class_decl_res.class_name = p[2]

    if (p[3] is None): # handle extends class
        class_decl_res.super_class_name = ""
    else:
        class_decl_res.super_class_name = p[3]


    print("inside p_class_declaration")
    for el in p[5]:
        if type(el) == FieldRecord:
            # update colass name
            el.containing_class = class_decl_res.class_name
            class_decl_res.fields.append(el)
        elif type(el) == MethodRecord:
            class_decl_res.methods.append(el)
        else:
            class_decl_res.class_constructors.append(el)
    
    p[0] = class_decl_res

def p_optionalExtendsId(p):
    '''
    optionalExtendsId : EXTENDS ID
                | empty
    '''
    if len(p) == 3:
        # print("in optional " + str(p[1]) + str(p[2]))
        p[0] = str(p[2])
    else:
        # print("in optional " + str(p[1]))
        p[0] = None


def p_classBodyDeclPlus(p):
    '''
    class_body_decl_plus : class_body_decl
                        | class_body_decl class_body_decl_plus
    '''
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[2]

def p_class_body_decl(p):
    '''
    class_body_decl : field_decl
                    | method_decl
                    | constructor_decl
    '''
    if p[1]:
        if p[1][0] == "field_decl":
            modifier,var_decl = p[1][1],p[1][2]
            # print("inside p_class_body_decl " + str(modifier) + str(var_decl))
            resFR = FieldRecord()

            theType, theVar = var_decl
            resFR.type = theType
            resFR.field_name = theVar[0]

            if (modifier[0] is None): # no public/private
                # default is private
                resFR.field_visibility = "private"
            else:
                resFR.field_visibility = modifier[0]
            
            # print("mofofofof" + str(modifier[1]))
            if (modifier[1] is None): # no static
                resFR.field_applicability = "instance"
            else:
                resFR.field_applicability = "static"
            
            p[0] = resFR
            return
        elif p[1][0] == "method_decl":
            # print("inside p_class_body_decl " + "this is a method decl")
            p[0] = MethodRecord()
    else: # constructor_decl
        # a;lsijf;lasdfj;l
        p[0] = ConstructorRecord()
    

# fields
def p_field_decl(p):
    '''
    field_decl : modifier var_decl
    '''
    # print("inside p_field_decl" + str(p[1]) + str(p[2]))
    print("inside p_field_decl" + str(p[1]) + str(p[2][0]))
    p[0] = ("field_decl",p[1],p[2])

def p_modifier(p):
    '''
    modifier : public_private_zero_or_one static_zero_or_one
    '''
    # print("inside p_modifier" + str(p[1]) + str(p[2]))
    p[0] = (p[1],p[2])


def p_public_private_zero_or_one(p):
    '''
    public_private_zero_or_one : PUBLIC
                                | PRIVATE
                                | empty
    '''
    p[0] = p[1]

def p_static_zero_or_one(p):
    '''
    static_zero_or_one : STATIC
                        | empty
    '''
    p[0] = p[1]

def p_var_decl(p):
    '''
    var_decl : type variables SEMI_COLON
    '''
    # bunch of VariableRecords
    print("the example var decl " + str(p[1]) + str(p[2]))
    p[0] = (p[1],p[2])


def p_type(p):
    '''
    type : INT
        | FLOAT
        | BOOLEAN
        | ID
    '''
    p[0] = TypeRecord(p[1])

def p_variables(p):
    '''
        variables : variable additional_variables
    '''
    p[0] = [p[1]] + p[2]

def p_additional_variables(p):
    '''
    additional_variables : COMMA variable additional_variables
                        | empty
    '''
    if len(p) == 4:
        p[0] = [p[2]] + p[3]
    else:
        p[0] = []

def p_variable(p):
    '''
    variable : ID
    '''
    p[0] = p[1]

def p_method_decl(p):
    '''
    method_decl : modifier type ID  L_PAREN zero_or_one_formals R_PAREN block
                |   modifier VOID ID  L_PAREN zero_or_one_formals R_PAREN block
    
    '''
    p[0] = ("method_decl",p[1],p[2],p[3],p[5])

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
    thetype,theval = p[1]
    print("fooo" + str(thetype) + str(theval))
    match thetype:
        case "INT_CONST":
            p[0] = ConstantExpression(const_type = "Integer-constant",const_val = theval,line_range = [])


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
    # to change later a;lksdfj;askdjf;lakj;lsdkjf;
    p[0] = p[1]

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
    # to change later a;lsdjf;alsdkjf;alskdjf
    print("testingggggg" + str(p[1]))
    p[0] = p[1]

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
    # to change ;lkate ra;sdfjl;aksjdfl;aksldjf
    p[0] = p[1]

def p_assign(p):
    '''
    assign : lhs ASSIGNMENT_OP expr
            | lhs INCR_OP
            | INCR_OP lhs
            | lhs DECR_OP
            | DECR_OP lhs
    '''
    # to change a;lskdjfa;sldkjf;alskj;asldkjf
    # p[0] = AssignExpression()

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
