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
  
  def add_field_to_class_record(self,theField):
    self.fields.append(theField)
  
  def __str__(self):
    class_record_res_string = "Class Name: " + str(self.class_name) + "\n"
    class_record_res_string += "Superclass Name: " + str(self.super_class_name) + "\n"
    class_record_res_string += "Fields: " + "\n"

    for f in self.fields:
      class_record_res_string += str(f);

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

class ExpressionRecord:
  def __init__(self,expr_type,line_range):
    self.expr_type = expr_type
    self.line_range = line_range
  def __str__(self):
    return f'Expr( {self.get_content()} )'
  def get_content(self):
    return "" # empty result

# begin ExpressionRecord subclasses

class ConstantExpression(ExpressionRecord):
  def __init__(self,const_type,const_val,line_range):
    super().__init__("CONSTANT",line_range)
    self.const_type = const_type
    self.const_val = const_val
  def get_content(self):
    return f"Constant({self.const_type}({self.const_val}))"

class VarExpression(ExpressionRecord):
  pass

class UnaryExpression(ExpressionRecord):
  pass

class BinaryExpression(ExpressionRecord):
  pass

class AssignExpression(ExpressionRecord):
  def __init__(self,lhs_expr,rhs_expr,line_range):
    super().__init__(self,"ASSIGN",line_range)
    self.lhs_expr = lhs_expr
    self.rhs_expr = rhs_expr
  def get_content(self):
    return f"Assign({self.lhs}, {self.rhs})"

class AutoExpression(ExpressionRecord):
  pass

class FieldAccessExpression(ExpressionRecord):
  pass

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