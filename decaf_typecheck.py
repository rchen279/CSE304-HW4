from decaf_ast import *
import sys
class TypeChecker:
  # some helper stuff
  current_constructor_variable_table = None
  def __init__(self,ast):
    self.ast = ast
    self.type_checking_errors = []
  def isSubtype(self,e1,e2):
    '''
    e1, e2 [TypeRecord()] -> True/ False

    The type-checker will rely on a subtype relation defined over types. This relation follows the discussion in
    class, with one addition: for the purposes of type checking, we will consider int to be a subtype of float. In
    summary:
    • Type T is a subtype of itself (i.e., the subtype relation is reflexive).
    • int is a subtype of float
    • user(A) is a subtype of user(B) if A is a subclass of B.
    • null is a subtype of user(A) for any class A
    • class-literal(A) is a subtype of class-literal(B) if A is a subclass of B.
    '''
    print("inside is subtype for arguments ")
    print(str(e1) + str(type(e1)))
    print(str(e2) + str(type(e2)))

    if e1 == e2:
      print("Type T is a subtype of itself")
      return True
    elif e1 == TypeRecord("int") and e2 == TypeRecord("float"):
      print("int is a subtype of float")
      return True


    return False
  
  # Resolve the "isTypeCorrect" field to be true or false for each statement record
  def resolveStatementRecordTypeCorrect(self,stmt_record):
    ''' 
    1. If-stmt: This statement is type correct if the condition part is of boolean type, and the "Then" and "Else"
    parts are type correct.
    2. While-stmt: This statement is type correct if the condition is of boolean type and the loop body is type
    correct.
    3. For-stmt: This statement is type correct if the condition part is of boolean type and the initializer
    expression, update expression, and the loop body are all type correct.
    4. Return-stmt: This statement is type correct if the type of the expression matches the declared return type
    of the current method.
    o If the expression is empty, then the declared return type of the method must be void (and vice
    versa).
    o If the expression is not empty, then it must be type correct, and its type must be a sub-type of the
    declared return type of the method.

    5. Expr-stmt: This statement is type correct if the expression is type correct.
    6. Block-stmt: This statement is type correct if all the statements in the sequence are type correct.
    '''
    
    # print("inside checkStatementRecordType")
    # print("the type of stmt_record provided is " + str(type(stmt_record)))
    if isinstance(stmt_record,IfStmt):
      print("resolving if statement")
      # type correct if the condition is boolean type, and the "Then" and "Else"
      # parts are type correct.
      # stmt_record.isTypeCorrect = True if
      self.resolveExpressionRecordType(stmt_record.condition)
      cond_is_boolean = stmt_record.condition.type == TypeRecord("boolean")
      then_else_type_correct = True

      # isTypeCorrect 
      # self.resolveStatementRecordTypeCorrect(stmt_record.thenStmt)
      # self.resolveStatementRecordTypeCorrect(stmt_record.elseStmt)
      

      # thenClause = stmt_record.thenStmt
      # if (stmt_record.elseStmt is not None):
      #   print("there is an else")
      #   # resolve else stmt
      # else:
      #   print("no else detected ")
      

    elif isinstance(stmt_record,WhileStmt):
      pass
    elif isinstance(stmt_record,ForStmt):
      pass
    elif isinstance(stmt_record,ReturnStmt):
      pass
    elif isinstance(stmt_record,ExprStmt):
      # assign expression, auto expression, method call expression
      print("handling expr stmt")
      self.resolveTypeCorrectExprStmt(stmt_record)

      print("now resolved .. ")
      print(stmt_record.isTypeCorrect)

    elif isinstance(stmt_record,BlockStmt):
      pass

    # check invariant before return
    self.checkStmtTypeCorrectInvariant(stmt_record.isTypeCorrect)
      
  def resolveTypeCorrectExprStmt(self,expr_smt) -> None:
    print("inside checkTypeCorrectExprStmt for " + str(expr_smt))

    # the expression -> 
    print(type(expr_smt))
    exp = expr_smt.expr

    if isinstance(exp,AssignExpression):
      # resolve lhs_expr type
      if exp.lhs_expr.type == None:
        print("Assign Expr lhs type was None...now resolving")
        self.resolveExpressionRecordType(exp.lhs_expr)
        if exp.lhs_expr.type == TypeRecord("error"):
          print("Resolve Failed: lhs of AssignExpr has invalid type")
          return False
      else:
        pass
      
      # resolve rhs_expr type
      if exp.rhs_expr.type == None:
        print("Assign Expr rhs type was None...now resolving")

        print(type(exp.rhs_expr))
        if isinstance(exp.rhs_expr, AssignExpression):
          print("Assign rhs is also an Assign, yet to be resolved")
        self.resolveExpressionRecordType(exp.rhs_expr)
      else:
        pass
      print("Assign Expr lhs and rhs done resolving are are as follows: ")
      print("lhs type : " + str(exp.lhs_expr.type))
      print("rhs type : " + str(exp.rhs_expr.type))

      # e1 and e2 are confirmed type correct
      # True if isSubtype(e2.type, e1.type)
      expr_smt.isTypeCorrect = self.isSubtype(exp.rhs_expr.type,exp.lhs_expr.type)
      
    # elif isinstance(exp,AutoExpression):
    #   print("Auto Expression........")
    # # else:
    # return True
    
  
  # Resolve the "type" field for each expression record
  def resolveExpressionRecordType(self,expression_record):
    '''
    1. Constant-expressions:
    o Integer-constant: has type int.
    o Float-constant: has type float.
    o String-constant: has type string.
    o True and False have type boolean.
    o Null has type null.
    2. Var-expression: has, as its type, the declared type of the corresponding variable.
    3. Unary-expressions:
    o uminus e: has the same type as e if e's type is int or float; has type error otherwise.
    o neg e: has type boolean if e's type is boolean; has type error otherwise.
    4. Binary-expressions:
    o Arithmetic operations: add, sub, mul, div: have type int if both operands have type int;
    otherwise, have type float if both operands have type int or float; otherwise have type
    error.
    o Boolean operations and, or: have type boolean if both operands have type boolean;
    otherwise have type error.
    o Arithmetic comparisons: lt, leq, gt, geq: have type boolean if both operands have type
    int or float; otherwise have type error.
    o Equality comparisons eq, neq: have type boolean if the type of one operand is a subtype of
    another; otherwise have type error.

    5. Assign-expression: Consider e1 = e2. The type of this expression is e2's type, provided:
    o e1 and e2 are type correct
    o e2's type is a subtype of e1's type.
    6. Auto-expression: has the same type as its argument expression e, if e's type is int or float; has
    type error otherwise.
    7. Field-access-expression: Let p.x be the field access expression, and let z be the field obtained by name
    resolution. Then this expression's type is the same as z's declared type, provided:
    o p's type is user(A), and z is a non-static field.
    o p's type is class-literal(A) and z is a static field.
    The type of this expression is error if name resolution has errors or the above conditions do not hold.
    8. Method-call-expression: Let p.f(e_1, ..., e_n) be the method call expression, and let h be the
    method obtained by name resolution. Then this expression's type is the same as h's declared return
    type, provided:
    o p's type is user(A), and h is a non-static method.
    o p's type is class-literal(A), and h is a static method.
    The type of this expression is error if name resolution had errors or the above conditions do not hold.
    9. New-object-expression: Let A(e_1, ..., e_n) be the constructor and arguments of this expression.
    The type of this expression is user(A) if name resolution succeeds (error, otherwise).
    10. This-expression: has type user(A), where A is the current class.
    11. Super-expression: has type user(B) if B is the immediate superclass of the current class; it's error if
    the current class has no superclass.
    12. Class-reference-expression: has type class-literal(A) where A is the literal class name in this
    expression (has type error if name resolution fails for this class name).
    '''

    if isinstance(expression_record,ConstantExpression):
      print("resolving constant expression")
      # ["", "", "","void","class-literal","error",""]
        #   ConstantExpression
        #   constExprType, const_val
        #   self.type = None (fill this in with TypeRecord)

        # # "String-constant": TypeRecord() _. to handle later

      e_type_to_type_record = {"Float-constant": TypeRecord("float"),
                                "Integer-constant": TypeRecord("int"),
                                "Null": TypeRecord("null"),
                                "True": TypeRecord("boolean"),
                                "False": TypeRecord("boolean")
                                }
      if expression_record.constExprType in e_type_to_type_record:
        expression_record.type = e_type_to_type_record[expression_record.constExprType]
      else:
        expression_record.type = TypeRecord(expression_record.constExprType)


      # print("-----------------------")
      # print(str(expression_record.constExprType) + " " + str(type(expression_record.constExprType)))
      # print(str(expression_record.const_val) + " " + str(type(expression_record.const_val)))
      # print(str(expression_record.type) + " " + str(type(expression_record.type)))
      # print("........................")

    elif isinstance(expression_record,VarExpression):
      print("var expression detected ")

      # print("the variable id is")
      # print(expression_record.var_id)

      # print("the var table is ")
      # print(TypeChecker.current_constructor_variable_table)

      # retrieve var record from constructor variable table -> want type of var

      theVarRecord = None
      for v_record in TypeChecker.current_constructor_variable_table.varTable:
        # items are VariableRecord (variable_name, variable_id, variable_kind, type)
        if expression_record.var_id == v_record.variable_name:
          theVarRecord = v_record
          break
      if theVarRecord:
        expression_record.type = theVarRecord.type
      else:
        pass

      if expression_record.type is None:
        print("could not resolve the var expression type, set to type error")
        expression_record.type = TypeRecord("error")


    elif isinstance(expression_record,UnaryExpression):
      print("Unary Expr Detected")
      # unary expression is (operand, operator, type)
      # resolve e type
      self.resolveExpressionRecordType(expression_record.operand)
      if expression_record.operator == "uminus":
        if (expression_record.operand.type == TypeRecord("int") or expression_record.operand.type == TypeRecord("float")):
          expression_record.type = expression_record.operand.type
        else:
          expression_record.type = TypeRecord("error")
      else: # operator == "neg"
        expression_record.type = (expression_record.operand.type if expression_record.operand.type == TypeRecord("boolean") else TypeRecord("error"))

    elif isinstance(expression_record,BinaryExpression):
      # expression_record.type = .... 

# 4. Binary-expressions:
#     o Arithmetic comparisons: lt, leq, gt, geq: have type boolean if both operands have type
#     int or float; otherwise have type error.
#     o Equality comparisons eq, neq: have type boolean if the type of one operand is a subtype of
#     another; otherwise have type error.
      
  #   BinaryExpression(operand1,operand2,bin_operator, type = None)

      print("inside Binary Expression")
      # resolve Binary Expression Type
      bin_op = expression_record.bin_operator
      # print("the bin op is ") # string
      # print(bin_op)

      # resolve operand types
      self.resolveExpressionRecordType(expression_record.operand1)
      self.resolveExpressionRecordType(expression_record.operand2)

      # TypeRecord()
      o1_type = expression_record.operand1.type
      o2_type = expression_record.operand2.type

      if bin_op in ["add","sub","mul","div"]:
        print("arithmetic operation detected")
        if (o1_type == TypeRecord("int") and o2_type == TypeRecord("int")):
          expression_record.type = TypeRecord("int")
        elif (o1_type == TypeRecord("int") and o2_type == TypeRecord("float")):
          expression_record.type = TypeRecord("float")
        elif (o1_type == TypeRecord("float") and o2_type == TypeRecord("int")):
          expression_record.type = TypeRecord("float")
        elif (o1_type == TypeRecord("float") and o2_type == TypeRecord("float")):
          expression_record.type = TypeRecord("float")
        else:
          expression_record.type = TypeRecord("error")

        # print("the resolved type for bin expr is ")
        # print(expression_record)
        # print(expression_record.type)

      elif bin_op in ["and","or"]:
        print("boolean detected")
        if (o1_type == TypeRecord("boolean") and o2_type == TypeRecord("boolean")):
          expression_record.type = TypeRecord("boolean")
        else:
          expression_record.type = TypeRecord("error")

        # print("the resolved type for bin expr is ")
        # print(expression_record)
        # print(expression_record.type)

      elif bin_op in ["lt","leq","gt","geq"]:
        print("arithmetic comparison detected ")
        o1_int_or_float = o1_type == TypeRecord("int") or o1_type == TypeRecord("float")
        o2_int_or_float = o2_type == TypeRecord("int") or o2_type == TypeRecord("float")
          
        if (o1_int_or_float and o2_int_or_float):
          expression_record.type = TypeRecord("boolean")
        else:
          expression_record.type = TypeRecord("error")

        # print("the resolved type for bin expr is ")
        # print(expression_record)
        # print(expression_record.type)

      else: # eq, neq
        print("equality comparison detected")
        # to be implemented

    elif isinstance(expression_record,AssignExpression):
      # print("assign expression detected")
      # resolve types for assign lhs rhs
      # if self.resolveTypeCorrectExprStmt(expression_record):
      #   expression_record.type = expression_record.rhs_expr.type
      # else:
      #   print("erooo")
      pass
      
    elif isinstance(expression_record,AutoExpression):
      print("auto expression detected")
      # resolve arg expr e type
      # print("the type before is ")
      # print(expression_record.operand.type)
      self.resolveExpressionRecordType(expression_record.operand)
      # print("the type after is")
      # print(expression_record.operand.type)

      e_type = expression_record.operand.type
      if e_type == TypeRecord("int"):
        expression_record.type = e_type
      elif e_type == TypeRecord("float"):
        expression_record.type = e_type
      else:
        expression_record.type = TypeRecord("error")
      # print("the resolved type for bin expr is ")
      # print(expression_record)
      # print(expression_record.type)

    elif isinstance(expression_record,FieldAccessExpression):
      # expression_record.type = .... 
      pass
    elif isinstance(expression_record,MethodCallExpression):
      # expression_record.type = .... 
      pass
    elif isinstance(expression_record,NewObjectExpression):
      # expression_record.type = .... 
      pass
    elif isinstance(expression_record,ThisExpression):
      # expression_record.type = .... 
      pass
    elif isinstance(expression_record,SuperExpression):
      # expression_record.type = .... 
      pass
    elif isinstance(expression_record,ClassReferenceExpression):
      # expression_record.type = .... 
      pass
    
    # check invariant before return
    self.checkResolveExprRecTypeInvariant(expression_record.type)

  def resolveConstructor(self,constructor):
    # constructor -> ConstructorRecord()
    for params in constructor.constructor_parameters: # <class 'list'>
      # print(params)
      pass
    for var in constructor.variable_table.varTable: # <class 'list'>
      # <class 'decaf_ast.VariableRecord'>
      # print(type(var))
      pass
    for b_smt in constructor.constructor_body.block_stmts:
      # b_smt instances of -> IfStmt, WhileStmt, ForStmt, ReturnStmt, ExprStmt, BreakStmt, ContinueStmt, BlockStmt
      # print("inside constructor body block stmts ----------------------------------")
      
      if type(b_smt) in [IfStmt,WhileStmt,ForStmt,ReturnStmt,ExprStmt,BlockStmt]:
        # resolve b_smt.isTypeCorrect -> True/False
        print("now checking statement " + str(b_smt) + " " + str(type(b_smt)))
        self.resolveStatementRecordTypeCorrect(b_smt)
        if b_smt.isTypeCorrect == False:
          print("b_smt has error resolving type -- should we abort??? ")
          raise ValueError("bad statement")
      else:
        print("No need to check for constructor body b_smt -- " + str(b_smt))

  def resolveMethod(self,method):
    pass
  def resolveField(self,field):
    pass
  def run(self):
    print("running type checker")
    print("...............................................................")
    print("...............................................................")
    print("...............................................................")
    print("...............................................................")
    print("...............................................................")
    print("...............................................................")
    print("...............................................................")
    print("...............................................................")
    print("...............................................................")
    print("...............................................................")

    for class_record in self.ast.class_records:
      # constructors, methods, fields
      for constructor in class_record.class_constructors:
       # resolve name

       # map constructor id to its variable table
      #  self.class_constructor_variable_tables[constructor.constructor_id] = constructor.variable_table
        
        # assign the constructor variable table
        TypeChecker.current_constructor_variable_table = constructor.variable_table
        # print("updating..... ")
        # print(TypeChecker.current_constructor_variable_table)
        self.resolveConstructor(constructor)
      for method in class_record.methods:
        self.resolveMethod(method)
      for field in class_record.fields:
        self.resolveField(field)

  def checkStmtTypeCorrectInvariant(self,s):
    assert isinstance(s,bool)

  def checkResolveExprRecTypeInvariant(self,e):
    assert isinstance(e,TypeRecord)
