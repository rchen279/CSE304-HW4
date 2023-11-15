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
  def __init__(self):
    self.constructor_id = ""
    self.constructor_visibility = ""
    self.constructor_parameters = []
    self.variable_table = {}
    self.constructor_body = ""
  def __str__(self):
    return "This is a constructor record"

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
    return "This is a method record"
  
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

class VariableTable:
  pass

class VariableRecord:
  def __init__(self):
    self.variable_name = ""
    self.variable_id = ""
    self.variable_kind = ""
    self.type = ""
  def __str__(self):
    return "This is a variable record"

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
# 1. If-stmt: has three pieces of information:
# • the condition of the "if", which is an expression (described later),
# • the "then" part, which is another statement, and
# • the "else" part, which is another statement.
# 2. While-stmt: has two pieces of information:
# • the loop-condition of the "while", which is an expression (described later),
# • the loop body, which is another statement.
# 3. For-stmt: has four pieces of information:
# • the initializer expression, which is an expression (described later),
# • the loop condition, which is another expression (described later),
# • the update expression, which is another expression (described later),
# • the loop body, which is another statement.
# 4. Return-stmt: has one optional piece of information:
# • the return value, specified by an expression (described later).
# 5. Expr-stmt: has one piece of information:
# • the expression that comprises of this statement.
# 6. Block-stmt: has one piece of information:

# • a sequence of statements that comprise of this block.
# 7. Break-stmt: representing break.
# 8. Continue-stmt: representing continue.
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
  pass

class UnaryExpression(ExpressionRecord):
  pass

class BinaryExpression(ExpressionRecord):
  pass

class AssignExpression(ExpressionRecord):
  def __init__(self,lhs_expr,rhs_expr,line_range):
    super().__init__(self,line_range)
    self.lhs_expr = lhs_expr
    self.rhs_expr = rhs_expr
  def get_content(self):
    return f"Assign({self.lhs}, {self.rhs})"

class AutoExpression(ExpressionRecord):
  pass

class FieldAccessExpression(ExpressionRecord):
  def __init__(self,baseExpr,fieldName,line_range):
    super().__init__(line_range)
    self.baseExpr = baseExpr
    self.fieldName = fieldName
  def __str__(self):
    return f"Field-access({self.baseExpr}, {self.fieldName}),"

class MethodCallExpression(ExpressionRecord):
  pass

class NewObjectExpression(ExpressionRecord):
  pass

class ThisExpression(ExpressionRecord):
  pass

class SuperExpression(ExpressionRecord):
  pass

class ClassReferenceExpression(ExpressionRecord):
  pass

# End ExpressionRecord subclasses

# End Variable Table: Contents