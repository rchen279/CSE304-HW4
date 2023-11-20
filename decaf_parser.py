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
        p[2].add_class(p[1])
        print("now adding " + str(p[1].class_name))
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
        p[0] = str(p[2])
    else:
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
    # p[1] has type FieldRecord or MethodRecord or ConstructorRecord
    p[0] = p[1]
    

# fields
def p_field_decl(p):
    '''
    field_decl : modifier var_decl
    '''

    print(f"\ninside p_field_decl modifer is {str(p[1])} \n var_decl is {str(p[2])}")
    resultFr = FieldRecord()

    # to fix later -> p[2] now returns list of VariableRecords
    theType, theVar = ("foo","x")

    resultFr.type = theType
    resultFr.field_name = theVar[0]

    pubPriv = p[1][0]
    if (pubPriv is None): # no public/private
        # default is private
        resultFr.field_visibility = "private"
    else:
        resultFr.field_visibility = pubPriv

    hasStatic = p[1][1]
    if (hasStatic is None): # no static
        resultFr.field_applicability = "instance"
    else:
        resultFr.field_applicability = "static"

    p[0] = resultFr # FieldRecord obj


def p_modifier(p):
    '''
    modifier : public_private_zero_or_one static_zero_or_one
    '''
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
    # generate list of VariableRecords
    print("the example var decl " + str(p[1]) + str(p[2]))
    var_type = p[1]
    # variable kind will be filled in by above
    p[0] = [VariableRecord(v,var_type) for v in p[2]]

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
    p[0] = MethodRecord() # to be completed
    # print("inside p_method_decl" + str(p[7]))

def p_zero_or_one_formals(p):
    '''
    zero_or_one_formals : formals
                        | empty
    '''

def p_constructor_decl(p):
    '''
    constructor_decl : modifier ID L_PAREN zero_or_one_formals R_PAREN block
    '''
    print("inside p_constructor_decl " +
          str(p[1]) + str(p[2]) + str(p[4]) + str(p[6]))
    print(p[6])
    p[0] = ConstructorRecord()


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
    res = BlockStmt()
    print("inside p_block \n" + str(p[2]))
    for s in p[2]:
        res.append_stmt_to_block(s)
    p[0] = res

def p_stmt_star(p):
    '''
    stmt_star : stmt stmt_star
                | empty
    '''
    if len(p) == 3:
        p[0] = [p[1]] + p[2]
    else:
        p[0] = []

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
    # statement record sub class initialize here
    # print("inside p_stmt " + str(p[1]))
    p[0] = p[1]

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
    p[0] = p[1]

def p_zero_or_one_expr(p):
    '''
    zero_or_one_expr : expr
                    | empty
    '''
    p[0] = p[1]

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
    if (type(p[1]) == tuple): # float / int const
        constType,constVal = p[1]
        if constType == "FLOAT_CONST":
            p[0] = ConstantExpression("Float-constant",constVal,[])
        else: # int const
            p[0] = ConstantExpression("Integer-constant",constVal,[])
    else:
        match p[1]:
            case "null":
                p[0] = ConstantExpression("Null",None,[])
            case "true":
                p[0] = ConstantExpression("True",None,[])
            case "false":
                p[0] = ConstantExpression("False",None,[])
            case _ :
                p[0] = ConstantExpression("String-constant",p[1],[])

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
    # literal is of type ConstantExpression
    # this -> init 
    print("inside p_primary "+ str(p[1]))




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
    if len(p) == 4:
        print("this is field access " + str(p[1]) + str(p[3]))
        p[0] = FieldAccessExpression(p[1],p[3],[])
    else:
        print(" this is len " + f"{p[1]}")
def p_method_invocation(p):
    '''
    method_invocation : field_access L_PAREN zero_or_one_arguments R_PAREN
    '''
    # method call expression

def p_expr(p):
    '''
    expr : primary
        | assign
        | expr arith_op expr
        | expr bool_op expr
        | unary_op expr
    '''
    p[0] = p[1]

def p_assign(p):
    '''
    assign : lhs ASSIGNMENT_OP expr
            | lhs INCR_OP
            | INCR_OP lhs
            | lhs DECR_OP
            | DECR_OP lhs
    '''
    # AssignExpression()
    print("this is assign")

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
