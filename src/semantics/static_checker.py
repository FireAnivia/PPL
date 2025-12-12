"""
Static Semantic Checker for OPLang Programming Language
"""

from typing import List, Dict, Optional, Any
from ..utils.visitor import ASTVisitor
from ..utils.nodes import *
from .static_error import *


class Symbol:
    def __init__(self, name, type, kind, is_final=False, is_static=False, value=None):
        self.name = name
        self.type = type
        self.kind = kind  # 'Variable', 'Constant', 'Attribute', 'Class', 'Method', 'Parameter'
        self.is_final = is_final
        self.is_static = is_static
        self.value = value

class ClassInfo:
    def __init__(self, name, super_name=None):
        self.name = name
        self.super_name = super_name
        self.attributes = {}
        self.methods = {} 

class StaticChecker(ASTVisitor):
    def __init__(self):
        self.global_classes = {}
        self.current_class = None
        self.current_method = None
        self.scope_stack = []
        self.in_loop = 0
        self.found_entry_point = False

    def check_program(self, ast):
        self.global_classes = {}
        self.current_class = None
        self.current_method = None
        self.scope_stack = []
        self.in_loop = 0
        self.found_entry_point = False
        return self.visit(ast, None)

    # --- Helpers ---
    def enter_scope(self):
        self.scope_stack.append({})

    def exit_scope(self):
        self.scope_stack.pop()

    def declare_symbol(self, name, symbol_type, kind, is_final=False, is_static=False, value=None):
        current_scope = self.scope_stack[-1]
        if name in current_scope:
            raise Redeclared(kind, name)
        current_scope[name] = Symbol(name, symbol_type, kind, is_final, is_static, value)

    def lookup(self, name):
        # 1. Tìm trong scope cục bộ (biến, tham số)
        for scope in reversed(self.scope_stack):
            if name in scope:
                return scope[name]
        
        # 2. Tìm trong thuộc tính của lớp hiện tại (và lớp cha)
        if self.current_class:
            attr = self.lookup_attribute(self.current_class.name, name)
            if attr:
                return attr
        return None

    def lookup_attribute(self, class_name, attr_name):
        if class_name not in self.global_classes:
            return None
        cls_info = self.global_classes[class_name]
        if attr_name in cls_info.attributes:
            return cls_info.attributes[attr_name]
        if cls_info.super_name:
            return self.lookup_attribute(cls_info.super_name, attr_name)
        return None

    def lookup_method(self, class_name, method_name):
        if class_name not in self.global_classes:
            return None
        cls_info = self.global_classes[class_name]
        if method_name in cls_info.methods:
            return cls_info.methods[method_name]
        if cls_info.super_name:
            return self.lookup_method(cls_info.super_name, method_name)
        return None

    def check_type_compatibility(self, type_lhs, type_rhs):
        if isinstance(type_lhs, PrimitiveType) and type_lhs.type_name == 'void': return False
        if isinstance(type_rhs, PrimitiveType) and type_rhs.type_name == 'void': return False

        if type(type_lhs) == type(type_rhs):
            if isinstance(type_lhs, PrimitiveType):
                if type_lhs.type_name == type_rhs.type_name: return True
                if type_lhs.type_name == 'float' and type_rhs.type_name == 'int': return True
            elif isinstance(type_lhs, ArrayType):
                return (type_lhs.size == type_rhs.size and 
                        self.check_type_compatibility(type_lhs.element_type, type_rhs.element_type))
            elif isinstance(type_lhs, ClassType):
                if type_lhs.class_name == type_rhs.class_name: return True
                return self.is_subtype(type_rhs.class_name, type_lhs.class_name)
            elif isinstance(type_lhs, ReferenceType):
                 return self.check_type_compatibility(type_lhs.referenced_type, type_rhs.referenced_type)

        if isinstance(type_lhs, ReferenceType):
            return self.check_type_compatibility(type_lhs.referenced_type, type_rhs)
        if isinstance(type_rhs, ReferenceType):
            return self.check_type_compatibility(type_lhs, type_rhs.referenced_type)
            
        if isinstance(type_lhs, ClassType) and isinstance(type_rhs, PrimitiveType) and type_rhs.type_name == 'nil':
             return True

        return False

    def is_subtype(self, child_name, parent_name):
        if child_name == parent_name: return True
        if child_name not in self.global_classes: return False
        current = self.global_classes[child_name]
        while current.super_name:
            if current.super_name == parent_name: return True
            if current.super_name not in self.global_classes: return False
            current = self.global_classes[current.super_name]
        return False

    def is_constant_expression(self, expr):
        if isinstance(expr, (IntLiteral, FloatLiteral, BoolLiteral, StringLiteral)):
            return True
        if isinstance(expr, UnaryOp):
            return self.is_constant_expression(expr.operand)
        if isinstance(expr, BinaryOp):
            return self.is_constant_expression(expr.left) and self.is_constant_expression(expr.right)
        if isinstance(expr, Identifier):
            sym = self.lookup(expr.name)
            return sym and (sym.kind == 'Constant' or (sym.kind == 'Attribute' and sym.is_final))
        return False

    # --- Visitor Methods ---

    def visit_program(self, node, o=None):
        for cls in node.class_decls:
            if cls.name in self.global_classes:
                raise Redeclared("Class", cls.name)
            self.global_classes[cls.name] = ClassInfo(cls.name, cls.superclass)

        for name, cls_info in self.global_classes.items():
            if cls_info.super_name and cls_info.super_name not in self.global_classes:
                raise UndeclaredClass(cls_info.super_name)

        for cls in node.class_decls:
            self.visit(cls, None)

        if not self.found_entry_point:
            raise NoEntryPoint()

    def visit_class_decl(self, node, o=None):
        self.current_class = self.global_classes[node.name]
        
        for member in node.members:
            if isinstance(member, AttributeDecl):
                for attr in member.attributes:
                    if attr.name in self.current_class.attributes:
                        raise Redeclared("Attribute", attr.name)
                    sym = Symbol(attr.name, member.attr_type, "Attribute", member.is_final, member.is_static)
                    self.current_class.attributes[attr.name] = sym
            elif isinstance(member, MethodDecl):
                if member.name in self.current_class.methods:
                    raise Redeclared("Method", member.name)
                self.current_class.methods[member.name] = member
                
                if (member.name == "main" and member.is_static and 
                    isinstance(member.return_type, PrimitiveType) and 
                    member.return_type.type_name == "void" and len(member.params) == 0):
                    self.found_entry_point = True

        for member in node.members:
            self.visit(member, None)
        
        self.current_class = None

    def visit_attribute_decl(self, node, o=None):
        for attr in node.attributes:
            if attr.init_value:
                type_rhs = self.visit(attr.init_value, None)
                if not self.check_type_compatibility(node.attr_type, type_rhs):
                    if node.is_final: raise TypeMismatchInConstant(node)
                    else: raise TypeMismatchInStatement(node)
                
                if node.is_final:
                    if not self.is_constant_expression(attr.init_value):
                        raise IllegalConstantExpression(attr.init_value)

    def visit_attribute(self, node, o=None): pass

    def visit_method_decl(self, node, o=None):
        self.current_method = node
        self.enter_scope()
        
        param_names = set()
        for param in node.params:
            if param.name in param_names:
                raise Redeclared("Parameter", param.name)
            param_names.add(param.name)
            self.declare_symbol(param.name, param.param_type, "Parameter")
            
        self.visit(node.body, None)
        self.exit_scope()
        self.current_method = None

    def visit_constructor_decl(self, node, o=None):
        self.enter_scope()
        for param in node.params:
            self.declare_symbol(param.name, param.param_type, "Parameter")
        self.visit(node.body, None)
        self.exit_scope()

    def visit_destructor_decl(self, node, o=None):
        self.enter_scope()
        self.visit(node.body, None)
        self.exit_scope()

    def visit_parameter(self, node, o=None): pass

    def visit_primitive_type(self, node, o=None): return node
    def visit_array_type(self, node, o=None): return node
    def visit_class_type(self, node, o=None):
        if node.class_name not in self.global_classes: raise UndeclaredClass(node.class_name)
        return node
    def visit_reference_type(self, node, o=None): return node

    def visit_block_statement(self, node, o=None):
        self.enter_scope()
        for var_decl in node.var_decls:
            self.visit(var_decl, None)
        for stmt in node.statements:
            self.visit(stmt, None)
        self.exit_scope()

    def visit_variable_decl(self, node, o=None):
        if isinstance(node.var_type, ClassType):
            self.visit(node.var_type)

        for var in node.variables:
            self.declare_symbol(var.name, node.var_type, "Variable" if not node.is_final else "Constant", is_final=node.is_final)
            if var.init_value:
                type_rhs = self.visit(var.init_value, None)
                if not self.check_type_compatibility(node.var_type, type_rhs):
                    if node.is_final: raise TypeMismatchInConstant(node)
                    else: raise TypeMismatchInStatement(node)
                if node.is_final and not self.is_constant_expression(var.init_value):
                    raise IllegalConstantExpression(var.init_value)

    def visit_variable(self, node, o=None): pass

    def visit_assignment_statement(self, node, o=None):
        lhs_type = self.visit(node.lhs, o)
        rhs_type = self.visit(node.rhs, o)
        
        # Kiểm tra gán hằng cho IdLHS (x := 5)
        if isinstance(node.lhs, IdLHS):
            sym = self.lookup(node.lhs.name)
            if sym and sym.is_final:
                raise CannotAssignToConstant(node)
        
        # Kiểm tra gán hằng cho PostfixLHS (this.x := 5)
        if isinstance(node.lhs, PostfixLHS):
            expr = node.lhs.postfix_expr
            # Nếu truy cập thuộc tính (MemberAccess) thì cần kiểm tra thuộc tính đó có final không
            if expr.postfix_ops and isinstance(expr.postfix_ops[-1], MemberAccess):
                member_name = expr.postfix_ops[-1].member_name
                
                # Tìm kiểu của đối tượng chứa thuộc tính
                obj_type = self.visit(expr.primary, o)
                for i in range(len(expr.postfix_ops) - 1):
                    op = expr.postfix_ops[i]
                    if isinstance(op, MethodCall):
                        m_sym = self.lookup_method(obj_type.class_name, op.method_name)
                        obj_type = m_sym.return_type
                    elif isinstance(op, MemberAccess):
                        a_sym = self.lookup_attribute(obj_type.class_name, op.member_name)
                        obj_type = a_sym.type
                    elif isinstance(op, ArrayAccess):
                        obj_type = obj_type.element_type
                
                if isinstance(obj_type, ClassType):
                    attr = self.lookup_attribute(obj_type.class_name, member_name)
                    if attr and attr.is_final:
                        raise CannotAssignToConstant(node)

        if not self.check_type_compatibility(lhs_type, rhs_type):
            raise TypeMismatchInStatement(node)

    def visit_if_statement(self, node, o=None):
        cond_type = self.visit(node.condition, o)
        if not (isinstance(cond_type, PrimitiveType) and cond_type.type_name == 'boolean'):
            raise TypeMismatchInStatement(node)
        self.visit(node.then_stmt, o)
        if node.else_stmt:
            self.visit(node.else_stmt, o)

    def visit_for_statement(self, node, o=None):
        sym = self.lookup(node.variable)
        if not sym: raise UndeclaredIdentifier(node.variable)
        if sym.is_final: raise CannotAssignToConstant(node)
        if not (isinstance(sym.type, PrimitiveType) and sym.type.type_name == 'int'):
             raise TypeMismatchInStatement(node)

        type_start = self.visit(node.start_expr, o)
        type_end = self.visit(node.end_expr, o)

        if not (isinstance(type_start, PrimitiveType) and type_start.type_name == 'int'):
            raise TypeMismatchInStatement(node)
        if not (isinstance(type_end, PrimitiveType) and type_end.type_name == 'int'):
            raise TypeMismatchInStatement(node)

        self.in_loop += 1
        self.visit(node.body, o)
        self.in_loop -= 1

    def visit_break_statement(self, node, o=None):
        if self.in_loop <= 0: raise MustInLoop(node)

    def visit_continue_statement(self, node, o=None):
        if self.in_loop <= 0: raise MustInLoop(node)

    def visit_return_statement(self, node, o=None):
        if self.current_method is None: return
        ret_type_decl = self.current_method.return_type
        if node.value:
            ret_type_expr = self.visit(node.value, o)
            if isinstance(ret_type_decl, PrimitiveType) and ret_type_decl.type_name == 'void':
                 raise TypeMismatchInStatement(node)
            if not self.check_type_compatibility(ret_type_decl, ret_type_expr):
                raise TypeMismatchInStatement(node)
        else:
            if not (isinstance(ret_type_decl, PrimitiveType) and ret_type_decl.type_name == 'void'):
                raise TypeMismatchInStatement(node)

    def visit_method_invocation_statement(self, node, o=None):
        self.visit(node.method_call, o)

    def visit_id_lhs(self, node, o=None):
        sym = self.lookup(node.name)
        if not sym: raise UndeclaredIdentifier(node.name)
        return sym.type

    def visit_postfix_lhs(self, node, o=None):
        return self.visit(node.postfix_expr, o)

    def visit_binary_op(self, node, o=None):
        left = self.visit(node.left, o)
        right = self.visit(node.right, o)
        op = node.operator

        if op in ['+', '-', '*', '/']:
            if self.check_type_compatibility(PrimitiveType('float'), left) and \
               self.check_type_compatibility(PrimitiveType('float'), right):
                if (isinstance(left, PrimitiveType) and left.type_name == 'float') or \
                   (isinstance(right, PrimitiveType) and right.type_name == 'float') or op == '/':
                    return PrimitiveType('float')
                return PrimitiveType('int')
        if op in ['%', '\\']:
             if self.check_type_compatibility(PrimitiveType('int'), left) and \
                self.check_type_compatibility(PrimitiveType('int'), right): return PrimitiveType('int')
        if op == '^':
             if isinstance(left, PrimitiveType) and left.type_name == 'string' and \
                isinstance(right, PrimitiveType) and right.type_name == 'string': return PrimitiveType('string')
        if op in ['==', '!=']:
            if self.check_type_compatibility(left, right) or self.check_type_compatibility(right, left): return PrimitiveType('boolean')
        if op in ['<', '<=', '>', '>=']:
             if self.check_type_compatibility(PrimitiveType('float'), left) and \
               self.check_type_compatibility(PrimitiveType('float'), right): return PrimitiveType('boolean')
        if op in ['&&', '||']:
             if isinstance(left, PrimitiveType) and left.type_name == 'boolean' and \
                isinstance(right, PrimitiveType) and right.type_name == 'boolean': return PrimitiveType('boolean')
        raise TypeMismatchInExpression(node)

    def visit_unary_op(self, node, o=None):
        operand = self.visit(node.operand, o)
        op = node.operator
        if op in ['+', '-']:
            if self.check_type_compatibility(PrimitiveType('float'), operand): return operand
        if op == '!':
            if isinstance(operand, PrimitiveType) and operand.type_name == 'boolean': return PrimitiveType('boolean')
        raise TypeMismatchInExpression(node)

    def visit_postfix_expression(self, node, o=None):
        curr_type = self.visit(node.primary, o)
        
        for op in node.postfix_ops:
            if isinstance(op, MethodCall):
                if not isinstance(curr_type, ClassType): raise TypeMismatchInExpression(node)
                method_sym = self.lookup_method(curr_type.class_name, op.method_name)
                if not method_sym: raise UndeclaredMethod(op.method_name)
                if len(op.args) != len(method_sym.params): raise TypeMismatchInExpression(node)
                for i, arg in enumerate(op.args):
                    arg_type = self.visit(arg, o)
                    if not self.check_type_compatibility(method_sym.params[i].param_type, arg_type):
                        raise TypeMismatchInExpression(node)
                curr_type = method_sym.return_type

            elif isinstance(op, MemberAccess):
                if not isinstance(curr_type, ClassType): raise TypeMismatchInExpression(node)
                attr_sym = self.lookup_attribute(curr_type.class_name, op.member_name)
                if not attr_sym: raise UndeclaredAttribute(op.member_name)
                
                is_static_access = False
                if isinstance(node.primary, Identifier):
                    if node.primary.name in self.global_classes and not self.lookup(node.primary.name):
                        is_static_access = True
                
                if is_static_access:
                    if not attr_sym.is_static: raise IllegalMemberAccess(node)
                else:
                    if attr_sym.is_static: raise IllegalMemberAccess(node)
                curr_type = attr_sym.type

            elif isinstance(op, ArrayAccess):
                if not isinstance(curr_type, ArrayType): raise TypeMismatchInExpression(node)
                idx_type = self.visit(op.index, o)
                if not (isinstance(idx_type, PrimitiveType) and idx_type.type_name == 'int'):
                    raise TypeMismatchInExpression(node)
                curr_type = curr_type.element_type
        return curr_type

    def visit_object_creation(self, node, o=None):
        if node.class_name not in self.global_classes: raise UndeclaredClass(node.class_name)
        for arg in node.args: self.visit(arg, o)
        return ClassType(node.class_name)

    def visit_identifier(self, node, o=None):
        sym = self.lookup(node.name)
        if sym: return sym.type
        if node.name in self.global_classes: return ClassType(node.name)
        raise UndeclaredIdentifier(node.name)

    def visit_this_expression(self, node, o=None):
        return ClassType(self.current_class.name)

    def visit_parenthesized_expression(self, node, o=None):
        return self.visit(node.expr, o)

    def visit_int_literal(self, node, o=None): return PrimitiveType('int')
    def visit_float_literal(self, node, o=None): return PrimitiveType('float')
    def visit_bool_literal(self, node, o=None): return PrimitiveType('boolean')
    def visit_string_literal(self, node, o=None): return PrimitiveType('string')
    def visit_nil_literal(self, node, o=None): return PrimitiveType('nil')

    def visit_array_literal(self, node, o=None):
        if not node.value: raise IllegalArrayLiteral(node)
        first_type = self.visit(node.value[0], o)
        for elem in node.value[1:]:
            curr_type = self.visit(elem, o)
            if not self.check_type_compatibility(first_type, curr_type) or \
               not self.check_type_compatibility(curr_type, first_type):
                raise IllegalArrayLiteral(node)
        return ArrayType(first_type, len(node.value))

    # --- Abstract Methods (Bắt buộc phải có để tránh TypeError) ---
    def visit_method_invocation(self, node, o=None): pass
    def visit_static_member_access(self, node, o=None): pass
    def visit_static_method_invocation(self, node, o=None): pass
    def visit_method_call(self, node, o=None): pass
    def visit_member_access(self, node, o=None): pass
    def visit_array_access(self, node, o=None): pass