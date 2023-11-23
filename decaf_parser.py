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
    print("inside p_constructor_decl ")


    mod, theId, z_o_o_formals, block = p[1],p[2],p[4],p[6]
    res_c_r = ConstructorRecord()
    print("------------------------")
    if mod[0] is None:
        # private default
        res_c_r.constructor_visibility = "private"
    else:
        res_c_r.constructor_visibility = mod[0]
    
    # handle formal params
    # create variable table
    for b in block.block_stmts:
        print(str(b) + "\n")
    # handle block
    
    p[0] = res_c_r

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
    print("inside p_block \n")
    for s in p[2]:
        print(s)
        if type(s) == list:
            for s1 in s:
                res.append_stmt_to_block(s1)
        else:
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
    p[0] = p[1]
    # if
    # while
    # for
    # return
    # stmt_expr
    # break
    # continue
    print("fooooooo p[1]" + str(p[1]))
    if type(p[1]) == BlockStmt:
        print("block statement detected in p_stmt")
    if type(p[1]) == list:
        print("var decl list type detected ")
    if type(p[1]) == str and p[1] == ";":
        print("semi colon stmt detected " + str(p[1]))

    

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
    print("inside p_primary ")
    if type(p[1]) == ConstantExpression: # literal
        print(f"literal detected {p[1]}")
        p[0] = p[1] 
    elif p[1] == "this":
        print(f"this detected {p[1]}")
        p[0] = ThisExpression(p[1],[])
    elif p[1] == "super":
        print(f"super detected {p[1]}")
        p[0] = SuperExpression(p[1],[])
    elif len(p) == 4:
        print(f"(expr) detected {p[1]} {p[2]} {p[3]}")
        p[0] = p[2]
    elif len(p) == 6:
        print(f"New Object Detected {p[1]} {p[2]} {p[3]} {p[4]} {p[5]}")
        newObjExpr = NewObjectExpression(p[2])
        if p[4]:
            for a in p[4]:
                newObjExpr.append_argument_to_new_obj_expr(a)
        p[0] = newObjExpr
    elif type(p[1]) == FieldAccessExpression or type(p[1]) == VarExpression:
        print(f"lhs detected {p[1]} ")
        p[0] = p[1]
    else:
        # print(f"method invocation detected {p[1]}")
        p[0] = p[1]

def p_zero_or_one_arguments(p):
    '''
    zero_or_one_arguments : arguments
                        | empty
    '''
    p[0] = p[1]

def p_arguments(p):
    '''
    arguments : expr additional_expr
    '''
    p[0] = [p[1]] + p[2]

def p_additional_expr(p):
    '''
    additional_expr : COMMA expr additional_expr
                    | empty
    '''
    if len(p) == 4:
        p[0] = [p[2]] + p[3]
    else:
        p[0] = []

def p_lhs(p):
    '''
    lhs : field_access
    '''
    p[0] = p[1]

def p_field_access(p):
    '''
    field_access : primary DOT ID
                | ID
    '''
    if len(p) == 4:
        p[0] = FieldAccessExpression(p[1],p[3],[])
    else:
        p[0] = VarExpression(p[1]) # need to change, replace with var id 
def p_method_invocation(p):
    '''
    method_invocation : field_access L_PAREN zero_or_one_arguments R_PAREN
    '''
    # method call expression
    print(f"method call detecteddddd {p[1]} {p[3]}")

    mc_base = ""
    mc_method_name = ""
    if isinstance(p[1],FieldAccessExpression):
        mc_base = p[1].baseExpr
        mc_method_name = p[1].fieldName
    else: # VarExpression -> default base is this
        mc_base = "this"
        mc_method_name = p[1].var_id
    res = MethodCallExpression(mc_base,mc_method_name)

    if p[3]:
        for a in p[3]:
            res.append_expr_to_method_call_expr(a)
    p[0] = res

def p_expr(p):
    '''
    expr : primary
        | assign
        | expr arith_op expr
        | expr bool_op expr
        | unary_op expr
    '''

    print(f"inside p_expr")
    if len(p) == 2: # primary or assign
        p[0] = p[1]
    elif len(p) == 4:
        print("case arith/bool op")
        bin_op = ""
        # arithmetic binary operators (4)
        if p[2] == "+":
            bin_op = "add"
        elif p[2] == "-":
            bin_op = "sub"
        elif p[2] == "*":
            bin_op = "mul"
        elif p[2] == "/":
            bin_op = "div"
        # boolean binary operators (8)
        elif p[2] == "&&":
            bin_op = "and"
        elif p[2] == "||":
            bin_op = "or"
        elif p[2] == "==":
            bin_op = "eq"
        elif p[2] == "!=":
            bin_op = "neq"
        elif p[2] == "<":
            bin_op = "lt"
        elif p[2] == "<=":
            bin_op = "leq"
        elif p[2] == ">":
            bin_op = "gt"
        elif p[2] == ">=":
            bin_op = "geq"
        p[0] = BinaryExpression(bin_op,p[1],p[3])
    else:
        print("case unary op")
        unary_operator = ""
        if p[1] == "+":
            pass
        elif p[1] == "-":
            unary_operator = "uminus"
        else:
            unary_operator = "neg"
        p[0] = UnaryExpression(p[2],unary_operator)
    
    print(f"this is the resulting p0 -------- {p[0]}")
def p_assign(p):
    '''
    assign : lhs ASSIGNMENT_OP expr
            | lhs INCR_OP
            | INCR_OP lhs
            | lhs DECR_OP
            | DECR_OP lhs
    '''

    if len(p) == 4:
        p[0] = AssignExpression(p[1],p[3],[])
    else:
        # Auto Expression
        resAutoExpr = None
        print(f"{p[1]} {p[2]}")
        if p[2] == "++":
            resAutoExpr = AutoExpression(p[1],"inc","post")
        elif p[2] == "--":
            resAutoExpr = AutoExpression(p[1],"dec","post")
        elif p[1] == "++":
            resAutoExpr = AutoExpression(p[2],"inc","pre")
        else:
            resAutoExpr = AutoExpression(p[2],"dec","pre")
        p[0] = resAutoExpr

def p_arith_op(p):
    '''
    arith_op : PLUS
            | MINUS
            | TIMES
            | DIVIDE
    '''
    p[0] = p[1]


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
    p[0] = p[1]

def p_unary_op(p):
    '''
    unary_op : PLUS
            | MINUS
            | NEG_OP
    '''
    p[0] = p[1]

def p_stmt_expr(p):
    '''
    stmt_expr : assign
            | method_invocation
    '''
    print(f"inside stmt_expr is {p[1]}")
    p[0] = p[1]

# Error rule for syntax errors
def p_error(p):
    print("Syntax error in input!")
    print("Illegal Character '%s', at %d , %d" % (p.value[0], p.lineno, p.lexpos))
    print()
    sys.exit()
