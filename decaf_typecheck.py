# Ryan Chen 
# ryachen
# 113200236

from decaf_ast import *
class TypeChecker:
  # some helper stuff
  current_constructor_variable_table = None
  current_method_variable_table = None
  current_class_name = None
  current_method = None
  current_class_record = None
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
    if e1 == e2:
      # print("Type T is a subtype of itself")
      return True
    elif e1 == TypeRecord("int") and e2 == TypeRecord("float"):
      # print("int is a subtype of float")
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
    if isinstance(stmt_record,IfStmt):
      # print("resolving if statement")
      # condition is expr and all expr has type field
      if stmt_record.condition.type is None:
        self.resolveExpressionRecordType(stmt_record.condition)

      validIfCond = stmt_record.condition.type == TypeRecord("boolean")
      if (validIfCond == False):
        self.type_checking_errors.append("IF statement - Condition not boolean")

      tc_then = False
      if type(stmt_record.thenStmt) in [IfStmt,WhileStmt,ForStmt,ReturnStmt,ExprStmt,BlockStmt]:
        if stmt_record.thenStmt.isTypeCorrect is None:
          self.resolveStatementRecordTypeCorrect(stmt_record.thenStmt)
        tc_then = stmt_record.thenStmt.isTypeCorrect
        if tc_then == False:
          self.type_checking_errors.append("IF statement - Type error in THEN statement")
      else: # [BreakStmt,ContinueStmt,VarDecl (not in ast), Semicolon]
        tc_then = True

      tc_else = False
      if (stmt_record.elseStmt is None):
        # print("if stmt has no else branch")
        tc_else = True
      else:
        if type(stmt_record.elseStmt) in [IfStmt,WhileStmt,ForStmt,ReturnStmt,ExprStmt,BlockStmt]:
          if stmt_record.elseStmt.isTypeCorrect is None:
            self.resolveStatementRecordTypeCorrect(stmt_record.elseStmt)
          tc_else = stmt_record.elseStmt.isTypeCorrect
          if tc_else == False:
            self.type_checking_errors.append("IF statement - Type error in ELSE statement")
        else:
          tc_else = True

      stmt_record.isTypeCorrect = (validIfCond == True) and (tc_then == True) and (tc_else == True)

      # print("the if statement resolved to -> {f}".format(f=stmt_record.isTypeCorrect))

    elif isinstance(stmt_record,WhileStmt):
      # print("handle while stmt...")
      if(stmt_record.condition.type is None):
        self.resolveExpressionRecordType(stmt_record.condition)
      # body is block stmt
      if stmt_record.body.isTypeCorrect == None:
        self.resolveStatementRecordTypeCorrect(stmt_record.body)

      isBoolCond = stmt_record.condition.type == TypeRecord("boolean")
      if isBoolCond == False:
        self.type_checking_errors.append("WHILE statement - Condition not boolean")

      isBodyTC = stmt_record.body.isTypeCorrect
      if isBodyTC == False:
        self.type_checking_errors.append("WHILE statement - Type error in loop body")
      stmt_record.isTypeCorrect = isBoolCond and isBodyTC

      print("the while statement resolved to ---- {ws}".format(ws=stmt_record.isTypeCorrect))

    elif isinstance(stmt_record,ForStmt):
      # print("inside for expression")
      isCondBool = True
      tc_init = True
      tc_update = True
      tc_body = True

      # resolve type correctness of init
      if stmt_record.init is not None:
        # print("the init type before was {tr}".format(tr=stmt_record.init.type))
        tc_init = self.wrapAndResolveExprStmtTypeCorrect(stmt_record.init)
        # print("the init type after is {tr}".format(tr=stmt_record.init.type))
        # print("the type correctness also has resolved to... ")
        # print(tc_init)
        if tc_init == False:
          self.type_checking_errors.append("FOR statement - Type error in initializer")

      # resolve type correctness of update
      if stmt_record.update is not None:
        # print("the update type before was {tr}".format(tr=stmt_record.update.type))
        tc_update = self.wrapAndResolveExprStmtTypeCorrect(stmt_record.update)
        # print("the udpate type after is {tr}".format(tr=stmt_record.update.type))
        # print("the type correctness also has resolved to... ")
        # print(tc_update)
        if tc_update == False:
          self.type_checking_errors.append("FOR statement - Type error in update expression")
      
      
      # resolve type correct body
      if type(stmt_record.body) in [IfStmt,WhileStmt,ForStmt,ReturnStmt,ExprStmt,BlockStmt]:
        if (stmt_record.body.isTypeCorrect is None):
          self.resolveStatementRecordTypeCorrect(stmt_record.body)
        tc_body = stmt_record.body.isTypeCorrect
        if tc_body == False:
          self.type_checking_errors.append("FOR statement - Type error in loop body")
      else:
        pass # otherwise body is type correct
      
      if stmt_record.cond is not None:
        if stmt_record.cond.type is None:
          self.resolveExpressionRecordType(stmt_record.cond)
          isCondBool = stmt_record.cond.type == TypeRecord("boolean")
          if isCondBool == False:
            self.type_checking_errors.append("FOR statement - Condition not boolean")


      stmt_record.isTypeCorrect = ((isCondBool == True) and (tc_init == True) and (tc_update == True) and (tc_body == True))
      # print("for stmt resolved to tc {tc}".format(tc=stmt_record.isTypeCorrect))

    elif isinstance(stmt_record,ReturnStmt):
      # print("return stmt detected")
      mt_return = TypeChecker.current_method.return_type
      tc_return = True

      return_val = stmt_record.val
      if return_val is None:
        if mt_return is not None:
          tc_return = False
          self.type_checking_errors.append("RETURN statement - NON-VOID method returns nothing")
      else:
        if return_val.type is None:
          self.resolveExpressionRecordType(return_val)
        if self.isSubtype(return_val.type,mt_return) == False:
          tc_return = False
          self.type_checking_errors.append("RETURN statement - Type Mismatch")
      stmt_record.isTypeCorrect = tc_return
    elif isinstance(stmt_record,ExprStmt):
      # assign expression, auto expression, method call expression
      # print("handling expr stmt")
      tc_expr_stmt = False
      if isinstance(stmt_record.expr,AssignExpression):
        # print("assign expression detectdd")
        tc_expr_stmt = self.resolveAssignExprTypeCorrect(stmt_record.expr)
      elif isinstance(stmt_record.expr,AutoExpression):
        # print("auto expression detected")
        tc_expr_stmt = self.resolveAutoExprTypeCorrect(stmt_record.expr)
      else:
        # print("method call expression detected")
        tc_expr_stmt = self.resolveMethodCallExpressionTypeCorrect(stmt_record.expr)
      
      stmt_record.isTypeCorrect = tc_expr_stmt
      # print("the newly resolved auto expression is type correct? {tc}".format(tc=stmt_record.isTypeCorrect))

    elif isinstance(stmt_record,BlockStmt):
      # print("block stmt detected")
      tc_block_smt = True
      for stmt in stmt_record.block_stmts:
        if type(stmt) in [IfStmt,WhileStmt,ForStmt,ReturnStmt,ExprStmt,BlockStmt]:
          if stmt.isTypeCorrect == None:
            self.resolveStatementRecordTypeCorrect(stmt)
          if stmt.isTypeCorrect == False:
            # print("detected bad block stmt")
            tc_block_smt = False
            break
        else:
          # print("has no is type corr-> continueing ")
          continue
      stmt_record.isTypeCorrect = tc_block_smt
      # print("blocksmt has resolved to {tvalu}".format(tvalu=stmt_record.isTypeCorrect))

    # check invariant before return
    self.checkStmtTypeCorrectInvariant(stmt_record.isTypeCorrect)
      
  def resolveTypeCorrectExprStmt(self,expr_smt) -> None:
    # print("inside checkTypeCorrectExprStmt for " + str(expr_smt))

    # the expression -> 
    print(type(expr_smt))
    exp = expr_smt.expr

    if isinstance(exp,AssignExpression):

      # resolve lhs_expr type
      if exp.lhs_expr.type == None:
        # print("Assign Expr lhs type was None...now resolving")
        self.resolveExpressionRecordType(exp.lhs_expr)
        if exp.lhs_expr.type == TypeRecord("error"):
          # print("Resolve Failed: lhs of AssignExpr has invalid type")
          return False
      else:
        pass
      # resolve rhs_expr type
      if exp.rhs_expr.type == None:
        # print("Assign Expr rhs type was None...now resolving")
        print(type(exp.rhs_expr))
        if isinstance(exp.rhs_expr, AssignExpression):
          # print("Assign rhs is also an Assign, yet to be resolved")
          pass
        self.resolveExpressionRecordType(exp.rhs_expr)
      else:
        pass
      expr_smt.isTypeCorrect = self.isSubtype(exp.rhs_expr.type,exp.lhs_expr.type)
      
    # elif isinstance(exp,AutoExpression):
    #   print("Auto Expression........")
    # # else:
    # return True
    
  
  def wrapAndResolveExprStmtTypeCorrect(self,assign_or_auto_or_method_call)->bool:
    wrapped = ExprStmt(assign_or_auto_or_method_call)    
    self.resolveStatementRecordTypeCorrect(wrapped)
    return wrapped.isTypeCorrect

  def resolveAssignExprTypeCorrect(self,assignExp) -> bool:
    # print("inside resolveAssignExprTypeCorrect")
    # resolve the type for assign expr
    if assignExp.type == None:
      self.resolveExpressionRecordType(assignExp)
    # print("the resolved type for assign was {ss}".format(ss=assignExp.type))
    # not sure? assume e1 and e2 are type correct as they have been resolved
    # check if e2 is subtype of e1
    is_subtype = self.isSubtype(assignExp.rhs_expr.type,assignExp.lhs_expr.type)
    if is_subtype == False:
      self.type_checking_errors.append("Error in {ae}".format(ae=assignExp))
    return is_subtype

  def resolveMethodCallExpressionTypeCorrect(self,method_call_expr) -> bool:
    print("inside method call expr")
    print(method_call_expr)
    return True

  def resolveAutoExprTypeCorrect(self,autoexpr) -> bool:
    # print("inside resolve tc auto expr")
    if (autoexpr.type is None):
      self.resolveExpressionRecordType(autoexpr)
    return autoexpr.type != TypeRecord("error")
    

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
      # print("resolving constant expression")
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
      # print("var expression detected ")
      theVarTable = None
      if (TypeChecker.current_constructor_variable_table):
        theVarTable = TypeChecker.current_constructor_variable_table.varTable
      else:
        theVarTable = TypeChecker.current_method_variable_table.varTable

      theVarRecord = None
      for v_record in theVarTable:
        # items are VariableRecord (variable_name, variable_id, variable_kind, type)
        if expression_record.var_id == v_record.variable_name:
          theVarRecord = v_record
          break
      if theVarRecord:
        expression_record.type = theVarRecord.type
      else:
        pass

      if expression_record.type is None:
        # print("could not resolve the var expression type, set to type error")
        self.type_checking_errors.append("Could not resolve varexpr {ve}".format(ve=expression_record.var_id))
        expression_record.type = TypeRecord("error")


    elif isinstance(expression_record,UnaryExpression):
      # print("Unary Expr Detected")
      # unary expression is (operand, operator, type)
      # resolve e type
      if expression_record.operand.type is None:
        self.resolveExpressionRecordType(expression_record.operand)

      if expression_record.operator == "uminus":
        if (expression_record.operand.type == TypeRecord("int") or expression_record.operand.type == TypeRecord("float")):
          expression_record.type = expression_record.operand.type
        else:
          self.type_checking_errors.append("UNARY MINUS - Expression is not a number")
          expression_record.type = TypeRecord("error")
      else: # operator == "neg"
        expression_record.type = (expression_record.operand.type if expression_record.operand.type == TypeRecord("boolean") else TypeRecord("error"))
        if (expression_record.type == TypeRecord("error")):
          self.type_checking_errors.append("UNARY NEGATION - Expression is not boolean")

    elif isinstance(expression_record,BinaryExpression):
      # print("inside Binary Expression")
      bin_op = expression_record.bin_operator

      
      toErrStr = {
        "add": "BINARY ADDITION",
        "sub": "BINARY SUBTRACTION",
        "mul": "BINARY MULTIPLICATION",
        "div": "BINARY DIVISION",
        "and": "BINARY AND",
        "or": "BINARY OR",
        "lt": "BINARY LESS THAN",
        "leq": "BINARY LESS THAN OR EQUAL",
        "gt": "BINARY GREATER THAN",
        "geq": "BINARY GREATER THAN OR EQUAL",
        "eq": "BINARY EQUALITY", 
        "neq":"BINARY INEQUALITY"
      }

      # resolve operand types
      if expression_record.operand1.type is None:
        self.resolveExpressionRecordType(expression_record.operand1)
      if expression_record.operand2.type is None:
        self.resolveExpressionRecordType(expression_record.operand2)

      # TypeRecord()
      o1_type = expression_record.operand1.type
      o2_type = expression_record.operand2.type

      if bin_op in ["add","sub","mul","div"]:
        # print("arithmetic operation detected")
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
          self.type_checking_errors.append("{be} - Operand not a number".format(be=toErrStr[bin_op]))

      elif bin_op in ["and","or"]:
        # print("boolean detected")
        if (o1_type == TypeRecord("boolean") and o2_type == TypeRecord("boolean")):
          expression_record.type = TypeRecord("boolean")
        else:
          expression_record.type = TypeRecord("error")
          self.type_checking_errors.append("{be} - Operand not a boolean".format(be=toErrStr[bin_op]))

      elif bin_op in ["lt","leq","gt","geq"]:
        # print("arithmetic comparison detected ")
        o1_int_or_float = o1_type == TypeRecord("int") or o1_type == TypeRecord("float")
        o2_int_or_float = o2_type == TypeRecord("int") or o2_type == TypeRecord("float")
          
        if (o1_int_or_float and o2_int_or_float):
          expression_record.type = TypeRecord("boolean")
        else:
          expression_record.type = TypeRecord("error")
          self.type_checking_errors.append("{bin_e} - Operand not a number".format(bin_e=toErrStr[bin_op]))

      else: # eq, neq
        # print("equality comparison detected")
        if self.isSubtype(o1_type,o2_type) or self.isSubtype(o2_type,o1_type):
          expression_record.type = TypeRecord("boolean")
        else:
          expression_record.type = TypeRecord("error")
          self.type_checking_errors.append("{be} - Operands are not of congruent types".format(be=toErrStr[bin_op]))

    elif isinstance(expression_record,AssignExpression):
      # print("assign expression detected")
      # assign e1 = e2
      # resolve e1 type
      if (expression_record.lhs_expr.type is None):
        self.resolveExpressionRecordType(expression_record.lhs_expr)

      # resolve e2 type
      if(expression_record.rhs_expr.type is None):
        self.resolveExpressionRecordType(expression_record.rhs_expr)

      expression_record.type = expression_record.rhs_expr.type
      
    elif isinstance(expression_record,AutoExpression):
      # print("auto expression detected")
      toErrStr = "AUTO-INCREMENT" if expression_record.inc_or_dec == "inc" else "AUTO-DECREMENT"

      if (expression_record.operand.type is None):
        self.resolveExpressionRecordType(expression_record.operand)
      if (expression_record.operand.type == TypeRecord("int")) or (expression_record.operand.type == TypeRecord("float")):
        expression_record.type = expression_record.operand.type
      else:
        expression_record.type = TypeRecord("error")
        self.type_checking_errors.append("{ae} - Operand is not a number".format(ae=toErrStr))

    elif isinstance(expression_record,FieldAccessExpression):
      print("field access detected")

      pass
    elif isinstance(expression_record,MethodCallExpression):
      # expression_record.type = .... 
      pass
    elif isinstance(expression_record,NewObjectExpression):
      # expression_record.type = .... 
      pass
    elif isinstance(expression_record,ThisExpression):
      # print("this expression detected ")
      expression_record.type = TypeRecord(TypeChecker.current_class_name)

    elif isinstance(expression_record,SuperExpression):
      if TypeChecker.current_class_record.super_class_name is not None:
        expression_record.type = TypeRecord(TypeChecker.current_class_record.super_class_name)
      else:
        expression_record.type = TypeRecord("error")
        self.type_checking_errors.append("SUPER EXPRESSION - No super class")
      print(expression_record.type)
    elif isinstance(expression_record,ClassReferenceExpression):
      # expression_record.type = .... 
      pass
    
    # check invariant before return
    self.checkResolveExprRecTypeInvariant(expression_record.type)

  def resolveConstructor(self,constructor) -> bool:
    for params in constructor.constructor_parameters:
      pass
    for var in constructor.variable_table.varTable:
      pass
    
    if constructor.constructor_body.isTypeCorrect is None:
      self.resolveStatementRecordTypeCorrect(constructor.constructor_body)
    tc_constructor_body = constructor.constructor_body.isTypeCorrect
    return tc_constructor_body

  def resolveMethod(self,method_record) -> bool:
  #  print("inside the resolve function ehehhe")
    m_body = method_record.method_body
    if (m_body.isTypeCorrect is None):
      self.resolveStatementRecordTypeCorrect(m_body)
    tc_method_rec = m_body.isTypeCorrect
    return tc_method_rec

  def resolveField(self,field) -> bool:
    print("Now trying to resolve field")
    print(field)

  def run(self):
    print("Now running the type checker ..................... ")
    print()

    is_program_type_correct = True
    for class_record in self.ast.class_records:
      # print("handling class {cr}".format(cr=class_record.class_name))
      TypeChecker.current_class_record = class_record
      TypeChecker.current_class_name = class_record.class_name
      for constructor in class_record.class_constructors:
        TypeChecker.current_constructor_variable_table = constructor.variable_table
        tc_constructor = self.resolveConstructor(constructor)
        if tc_constructor == False:
          is_program_type_correct = False

      # done handling constructors
      TypeChecker.current_constructor_variable_table = None

      for method_record in class_record.methods:
        TypeChecker.current_method = method_record
        TypeChecker.current_method_variable_table = method_record.variable_table
        tc_method = self.resolveMethod(method_record)
        if tc_method == False:
          is_program_type_correct = False

      for field in class_record.fields:
        self.resolveField(field)

    print()
    print("Type Checker Finished")
    print("Is the program type correct? ")
    if(is_program_type_correct):
      print("Type Correct Program! Yay!")
    else:
      print("Nope. Something was wrong with the types ..")
    print("--------------------------")
    print("The following errors were detected:")
    for err in self.type_checking_errors:
      print(err)

  def checkStmtTypeCorrectInvariant(self,s):
    assert isinstance(s,bool)

  def checkResolveExprRecTypeInvariant(self,e):
    assert isinstance(e,TypeRecord)
