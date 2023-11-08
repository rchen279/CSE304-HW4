# code for ast file

# Start Class Table: Contents

class ClassTable: # a collection of class records
  def __init__(self):
    self.class_records = {}
    self.pErrors = [] # track errors in program
  def add_class(self,class_name,class_record): # adds a ClassRecord to the class table id by classname
    self.class_records[class_name] = class_record
  def get_class(self,class_name):
    return self.class_records.get(class_name,None)
  def __str__(self):
    res = "--------------------------------------------------------------------------\n"
    for class_name,class_record in self.class_records.items():
      res += str(class_record)
    return res

class ClassRecord:
  def __init__(self):
    self.class_name = ""
    self.super_class_name = ""
    self.class_constructors = []
    self.methods = []
    self.fields = []
  
  def __str__(self):
    class_record_res_string = "Class Name: " + str(self.class_name) + "\n"
    class_record_res_string += "Superclass Name: " + str(self.super_class_name) + "\n"
    class_record_res_string += "Fields: " + "\n"
    class_record_res_string += "Constructors: " + "\n"
    class_record_res_string += "Methods: " + "\n"
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
  def __init__(self):
    self.field_name = ""
    self.field_id = ""
    self.containing_class = ""
    self.field_visibility = ""
    self.field_applicability = ""
    self.type = ""
  def __str__(self):
    return "This is a field record"

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

class ExpressionRecord:
  def __init__(self,expr_type,line_range):
    self.expr_type = expr_type
    self.line_range = line_range
  def __str__(self):
    expr_record_res_string = ""
    
    return expr_record_res_string
# begin ExpressionRecord subclasses

class ConstantExpression(ExpressionRecord):
  pass

class VarExpression(ExpressionRecord):
  pass

class UnaryExpression(ExpressionRecord):
  pass



# 4. Binary-expression: has three pieces of information: the two operands (both expressions themselves) and
# the binary operator. Binary operators are one of add, sub, mul, div, and, or, eq, neq, lt, leq, gt,
# and geq.
# 5. Assign-expression: has two pieces of information: the left- and right- hand sides of the assignment (both
# expressions).
# 6. Auto-expression: has three pieces of information to represent auto-increment and auto-decrement
# expressions (e.g. "x++"). The three pieces of information are the operand (e.g. "x") which is an
# expression, whether the operation is an auto-increment (e.g. x++) or auto-decrement (e.g x--), and
# whether the operation is post (e.g. x++) or pre (e.g. ++x).
# 7. Field-access-expression: has two pieces of information to represent p.x: the base (e.g. p), which is an
# expression, and the field name (e.g. x), which is a string.
# 8. Method-call-expression: has three pieces of information to represent p.f(x, y): the base (e.g. p),
# which is an expression, the method name (e.g. x), which is a string, and a sequence of expressions
# representing the arguments to the method call (e.g. x, y). Note that the argument sequence may be
# empty.
# 9. New-object-expression: has two pieces of information for creating a new object as in new a(i, x):
# the base class name (e.g. a), and the sequence of expressions representing the arguments to the
# constructor (e.g. i, x). Note that the argument sequence may be empty.
# 10. This-expression: to denote this.
# 11. Super-expression: to denote super.
# 12. Class-reference-expression: with the referred class name as its information. This expression is used to
# denote the value of literal class names.

# begin ExpressionRecord subclasses

# End Variable Table: Contents