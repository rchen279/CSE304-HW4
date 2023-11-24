# code for ast file

# Start Class Table: Contents

class ClassTable: # a collection of class records
  def __init__(self):
    self.class_records = []
    self.pErrors = [] # track errors in program
  def add_class(self,class_record): # adds a ClassRecord to the class table id by classname
    self.class_records.append(class_record)
  def __str__(self):
    res = "--------------------------------------------------------------------------\n"
    for class_record in self.class_records:
      res += str(class_record)
    return res

class ClassRecord:
  def __init__(self):
    self.class_name = ""
    self.super_class_name = ""
    self.class_constructors = []
    self.methods = []
    self.fields = []
  
  def add_field_to_class_record(self,theField):
    self.fields.append(theField)
  
  def __str__(self):
    class_record_res_string = "Class Name: " + str(self.class_name) + "\n"
    class_record_res_string += "Superclass Name: " + str(self.super_class_name) + "\n"
    class_record_res_string += "Fields: " + "\n"

    for f in self.fields:
      class_record_res_string += str(f);

    class_record_res_string += "Constructors: " + "\n"

    for c in self.class_constructors:
      class_record_res_string += str(c);

    class_record_res_string += "Methods: " + "\n"

    for m in self.methods:
      class_record_res_string += str(m);

    class_record_res_string += "--------------------------------------------------------------------------\n"
    return class_record_res_string

class ConstructorRecord:
  constructorIdVal = 1
  def __init__(self):
    self.constructor_id = ConstructorRecord.constructorIdVal
    ConstructorRecord.constructorIdVal += 1
    self.constructor_visibility = ""
    self.constructor_parameters = []
    self.variable_table = VariableTable()
    self.constructor_body = ""
  def __str__(self):
    constructor_res_string = f"CONSTRUCTOR: {self.constructor_id}, {self.constructor_visibility}\n"
    constructor_res_string += "Constructor Parameters:\n"
    for cp in self.constructor_parameters:
      constructor_res_string += str(cp)
    constructor_res_string += "Variable Table:\n"
    if self.variable_table:
      constructor_res_string += str(self.variable_table)
    constructor_res_string += "Constructor Body:\n"
    constructor_res_string += self.constructor_body
    return constructor_res_string

class MethodRecord:
  def __init__(self):
    self.method_name = ""
    self.method_id = ""
    self.containing_class = ""
    self.method_visibility = ""
    self.method_applicability = ""
    self.method_parameters = []
    self.return_type = ""
    self.variable_table = {}
    self.method_body = ""
  def __str__(self):
    return "This is a method record\n"
  
class FieldRecord:
  fieldIdVal = 1
  def __init__(self):
    self.field_name = ""
    self.field_id = FieldRecord.fieldIdVal
    FieldRecord.fieldIdVal += 1
    self.containing_class = ""
    self.field_visibility = ""
    self.field_applicability = ""
    self.type = ""
  def __str__(self):
    return f"FIELD {self.field_id}, {self.field_name}, {self.containing_class}, {self.field_visibility}, {self.field_applicability}, {self.type}\n"

# End Class Table: Contents

# Start Variable Table: Contents

class VariableTable: # holds variable records
  def __init__(self):
    self.varTable = []
  def __str__(self):
    vt_res_string = ""
    for varRec in self.varTable:
      vt_res_string += varRec
    return vt_res_string


class VariableRecord:
  varIdVal = 1
  def __init__(self, variable_name, theType):
    self.variable_name = variable_name
    self.variable_id = VariableRecord.varIdVal
    VariableRecord.varIdVal += 1
    self.variable_kind = ""
    self.type = theType
  def __str__(self):
    return f"VARIABLE {self.variable_id}, {self.variable_name}, {self.variable_kind}, {self.type}"

class TypeRecord:
  def __init__(self,theType):
    self.type = theType
  def __str__(self):
    type_res_string = ""
    built_in_types_list = ["int", "float", "boolean","string"]
    if self.type in built_in_types_list:
      type_res_string = str(self.type)
    else:
      type_res_string = f'user({self.type})'
    return type_res_string

class StatementRecord:
  pass

# start statement record sub classes

# There are several kinds of statements, each with its own contents:


class IfStmt:
  def __init__(self,condition,thenStmt,elseStmt):
    self.condition = condition
    self.thenStmt = thenStmt
    self.elseStmt = elseStmt
  def __str__(self):
    return f"If-stmt( \n\tCondition: {self.condition},\n\tThen: {self.thenStmt}, \n\tElse: {self.elseStmt}\n)"


class WhileStmt:
  def __init__(self,condition,body):
    self.condition = condition
    self.body = body
  def __str__(self):
    return f"While-stmt(\n\tCondition: {self.condition},\n\tBody: {self.body})"

class ForStmt:
  def __init__(self,init,cond,update,body):
    self.init = init
    self.cond = cond
    self.update = update
    self.body = body
  def __str__(self):
    return f"For-stmt(\n\tInit: {self.init},\n\tCondition: {self.cond},\n\tUpdate: {self.update},\n\tBody: {self.body} \n)"

class ReturnStmt:
  def __init__(self,val=None):
    self.val = val
  def __str__(self):
    return f"Return-stmt(\n\t{self.val}\n)"

class ExprStmt:
  def __init__(self,expr):
    self.expr = expr
  def __str__(self):
    return f"Expr-stmt(\n\t{self.expr}\n)"

class BlockStmt:
  def __init__(self):
    self.block_stmts = []
  def append_stmt_to_block(self,stmt):
    self.block_stmts.append(stmt)
  def __str__(self):
    block_stmt_res_string = "Block(["
    for s in self.block_stmts:
      block_stmt_res_string += str(s) + ", "
    block_stmt_res_string += "])"
    return block_stmt_res_string

class BreakStmt:
  def __init__(self):
    pass
  def __str__(self):
    return f"Break-stmt()"
class ContinueStmt:
  def __init__(self):
    pass
  def __str__(self):
    return f"Continue-stmt()"
# 8. Continue-stmt: representing continue.

class SkipStmt:
  pass
# 9. Skip-stmt: representing an empty statement (as in an empty else part of an "if" statement).


# end statementRecord sub classes

class ExpressionRecord:
  def __init__(self,line_range):
    self.line_range = line_range
    self.expr_body = None # will fill with body
  def __str__(self):
    return f'Expr( {self.expr_body} )'

# begin ExpressionRecord subclasses
class ConstantExpression(ExpressionRecord):
  def __init__(self,constExprType,const_val,line_range):
    super().__init__(line_range)
    self.constExprType = constExprType
    self.const_val = const_val
  def __str__(self):
    return f"Constant({self.constExprType}({self.const_val}))"


class VarExpression(ExpressionRecord):
  def __init__(self,var_id): 
    self.var_id = var_id
  def __str__(self):
    return f"Variable({self.var_id})"

class UnaryExpression(ExpressionRecord):
  def __init__(self,operand,operator):
    self.operand = operand
    self.operator = operator
  def __str__(self):
    return f"Unary({self.operator}, {self.operand})"


class BinaryExpression(ExpressionRecord):
  def __init__(self,bin_operator,operand1,operand2):
    self.operand1 = operand1
    self.operand2 = operand2
    self.bin_operator = bin_operator
  def __str__(self):
    return f"Binary({self.bin_operator}, {self.operand1}, {self.operand2})"



class AssignExpression(ExpressionRecord):
  def __init__(self,lhs_expr,rhs_expr,line_range):
    self.lhs_expr = lhs_expr
    self.rhs_expr = rhs_expr
  def __str__(self):
    return f"Assign({self.lhs_expr}, {self.rhs_expr})"

# auto increment/ auto decrement
class AutoExpression(ExpressionRecord):
  def __init__(self,operand, inc_or_dec, post_or_pre):
    self.operand = operand
    self.inc_or_dec = inc_or_dec
    self.post_or_pre = post_or_pre
  def __str__(self):
    return f"Auto({self.operand}, {self.inc_or_dec}, {self.post_or_pre})"

class FieldAccessExpression(ExpressionRecord):
  def __init__(self,baseExpr,fieldName,line_range):
    super().__init__(line_range)
    self.baseExpr = baseExpr
    self.fieldName = fieldName
  def __str__(self):
    return f"Field-access({self.baseExpr}, {self.fieldName})"

class MethodCallExpression(ExpressionRecord):
  def __init__(self,base,method_name):
    self.base = base
    self.method_name = method_name
    self.expressions = []
  def append_expr_to_method_call_expr(self,expr):
    self.expressions.append(expr)
  def __str__(self):
    expr_res = [', '.join(str(a) for a in self.expressions)]
    return f"Method-call({self.base}, {self.method_name}, {expr_res})"

class NewObjectExpression(ExpressionRecord):
  def __init__(self,base_class_name):
    self.base_class_name = base_class_name
    self.arguments = []
  def append_argument_to_new_obj_expr(self,arg):
    self.arguments.append(arg)
  def __str__(self):
    return f"New-object({self.base_class_name}, [{', '.join(str(a) for a in self.arguments)}])"

class ThisExpression(ExpressionRecord):
  def __init__(self,content,line_range):
    self.content = content
  def __str__(self):
    return f"{self.content}"

class SuperExpression(ExpressionRecord):
  def __init__(self,content,line_range):
    self.content = content
  def __str__(self):
    return f"{self.content}"

class ClassReferenceExpression(ExpressionRecord):
  pass

# End ExpressionRecord subclasses

# End Variable Table: Contents