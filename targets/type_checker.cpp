#include <string>
#include "targets/type_checker.h"
#include ".auto/all_nodes.h"  // automatically generated
#include <cdk/types/primitive_type.h>

#define ASSERT_UNSPEC { if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC)) return; }

//---------------------------------------------------------------------------

bool mml::type_checker::deepTypeComparison(std::shared_ptr<cdk::basic_type> left, std::shared_ptr<cdk::basic_type> right) {
  // TODO: deal with functional types

  if (left->name() == cdk::TYPE_POINTER) {
    if (right->name() != cdk::TYPE_POINTER) {
      return false;
    }

    return deepTypeComparison(cdk::reference_type::cast(left)->referenced(), cdk::reference_type::cast(right)->referenced());
  } else if (right->name() == cdk::TYPE_POINTER) {
      return false;
  } else {
    return left == right;
  }
}

//---------------------------------------------------------------------------

void mml::type_checker::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}
void mml::type_checker::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}
void mml::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void mml::type_checker::do_sequence_node(cdk::sequence_node *const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void mml::type_checker::do_integer_node(cdk::integer_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}

//---------------------------------------------------------------------------

void mml::type_checker::processUnaryExpression(cdk::unary_operation_node *const node, int lvl, bool acceptDoubles) {
  ASSERT_UNSPEC;

  node->argument()->accept(this, lvl + 2);

  if (!node->argument()->is_typed(cdk::TYPE_INT)
        && !(acceptDoubles && node->argument()->is_typed(cdk::TYPE_DOUBLE))
        && !node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    throw std::string("wrong type in argument of unary expression");
  }

  node->type(node->argument()->type());
}

void mml::type_checker::do_neg_node(cdk::neg_node *const node, int lvl) {
  processUnaryExpression(node, lvl, true);
}

void mml::type_checker::do_identity_node(mml::identity_node *const node, int lvl) {
  processUnaryExpression(node, lvl, true);
}

void mml::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
  processUnaryExpression(node, lvl, false);
}

void mml::type_checker::do_alloc_node(mml::alloc_node *const node, int lvl) {
  ASSERT_UNSPEC;

  node->argument()->accept(this, lvl + 2);

  if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (!node->argument()->is_typed(cdk::TYPE_INT)) {
    throw std::string("wrong type in argument of unary expression");
  }

  node->type(cdk::reference_type::create(4, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC)));
}

//---------------------------------------------------------------------------

/**
 * @brief Process a binary arithmetic expression node.
 * 
 * The node will be typed as follows (commutative; argument type order is irrelevant):
 *     int, int -> int;
 *     int, double -> double;
 *     int, pointer -> pointer;
 *     double, double -> double;
 *     pointer, pointer -> int [both pointers must reference the same type].
 * 
 * If one argument is unspec and the other is not, both the node and the unspec argument will
 * be typed as the other argument's type. If both arguments are unspec, both will be typed as
 * int, and so will the node.
 * 
 * @param node the node to be processed
 * @param lvl the current visit level
 * @param acceptDoubles whether to accept doubles
 * @param acceptOnePointer whether to accept one argument, but not both, being a pointer
 * @param acceptBothPointers whether to accept both arguments being pointers; acceptOnePointer must be true
 */
void mml::type_checker::processBinaryArithmeticExpression(cdk::binary_operation_node *const node, int lvl,
      bool acceptDoubles, bool acceptOnePointer, bool acceptBothPointers) {
  ASSERT_UNSPEC;

  node->left()->accept(this, lvl + 2);

  if (node->left()->is_typed(cdk::TYPE_INT) || node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_INT) || (acceptDoubles && node->right()->is_typed(cdk::TYPE_DOUBLE))) {
      node->type(node->right()->type());
    } else if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (acceptOnePointer && node->right()->is_typed(cdk::TYPE_POINTER)) {
      node->type(node->right()->type());

      if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
        node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      }
    } else {
      throw std::string("wrong type in right argument of arithmetic binary expression");
    }

    if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
      node->left()->type(node->type());
    }
  } else if (acceptDoubles && node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_INT) || node->right()->is_typed(cdk::TYPE_DOUBLE)) {
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    } else if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
      node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
    } else {
      throw std::string("wrong type in right argument of arithmetic binary expression");
    }
  } else if (acceptOnePointer && node->left()->is_typed(cdk::TYPE_POINTER)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_INT)) {
      node->type(node->left()->type());
    } else if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      node->type(node->left()->type());
    } else if (acceptBothPointers && deepTypeComparison(node->left()->type(), node->right()->type())) {
      node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else {
      throw std::string("wrong type in right argument of arithmetic binary expression");
    }
  } else {
    throw std::string("wrong type in left argument of arithmetic binary expression");
  }
}

void mml::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  processBinaryArithmeticExpression(node, lvl, true, true, false);
}
void mml::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
  processBinaryArithmeticExpression(node, lvl, true, true, true);
}
void mml::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
  processBinaryArithmeticExpression(node, lvl, true, false, false);
}
void mml::type_checker::do_div_node(cdk::div_node *const node, int lvl) {
  processBinaryArithmeticExpression(node, lvl, true, false, false);
}
void mml::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  processBinaryArithmeticExpression(node, lvl, false, false, false);
}

void mml::type_checker::processBinaryPredicateExpression(cdk::binary_operation_node *const node, int lvl, bool acceptDoubles, bool acceptPointers) {
  ASSERT_UNSPEC;

  node->left()->accept(this, lvl + 2);
  
  if (node->left()->is_typed(cdk::TYPE_INT)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(node->left()->type());
    } else if (!node->right()->is_typed(cdk::TYPE_INT)
          && !(acceptDoubles && node->right()->is_typed(cdk::TYPE_DOUBLE))
          && !(acceptPointers && node->right()->is_typed(cdk::TYPE_POINTER))) {
      throw std::string("wrong type in right argument of arithmetic binary expression");
    }
  } else if (acceptDoubles && node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(node->left()->type());
    } else if (!node->right()->is_typed(cdk::TYPE_INT) && !node->right()->is_typed(cdk::TYPE_DOUBLE)) {
      throw std::string("wrong type in right argument of arithmetic binary expression");
    }
  } else if (acceptPointers && node->left()->is_typed(cdk::TYPE_POINTER)) {
    node->right()->accept(this, lvl + 2);

    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (!node->right()->is_typed(cdk::TYPE_INT) && !node->right()->is_typed(cdk::TYPE_POINTER)) {
      throw std::string("wrong type in right argument of arithmetic binary expression");
    }
  } else if (node->left()->is_typed(cdk::TYPE_UNSPEC)) {
    node->right()->accept(this, lvl + 2);
    
    if (node->right()->is_typed(cdk::TYPE_UNSPEC)) {
      node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
      node->right()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (node->right()->is_typed(cdk::TYPE_POINTER)) {
      node->left()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    } else if (node->right()->is_typed(cdk::TYPE_INT) || (acceptDoubles && node->right()->is_typed(cdk::TYPE_DOUBLE))) {
      node->left()->type(node->right()->type());
    } else {
      throw std::string("wrong type in right argument of arithmetic binary expression");
    }
  } else {
    throw std::string("wrong type in left argument of arithmetic binary expression");
  }

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void mml::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  processBinaryPredicateExpression(node, lvl, true, false);
}
void mml::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
  processBinaryPredicateExpression(node, lvl, true, false);
}
void mml::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  processBinaryPredicateExpression(node, lvl, true, false);
}
void mml::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  processBinaryPredicateExpression(node, lvl, true, false);
}
void mml::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  processBinaryPredicateExpression(node, lvl, true, false);
}
void mml::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  processBinaryPredicateExpression(node, lvl, true, false);
}
void mml::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
  processBinaryPredicateExpression(node, lvl, false, false);
}
void mml::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
  processBinaryPredicateExpression(node, lvl, false, false);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_address_of_node(mml::address_of_node *const node, int lvl) {
  ASSERT_UNSPEC;
  // TODO: implement this
  throw "not implemented";
}

//---------------------------------------------------------------------------

void mml::type_checker::do_variable_node(cdk::variable_node *const node, int lvl) {
  ASSERT_UNSPEC;
  const std::string &id = node->name();
  std::shared_ptr<mml::symbol> symbol = _symtab.find(id);

  if (symbol != nullptr) {
    node->type(symbol->type());
  } else {
    throw id;
  }
}

void mml::type_checker::do_pointer_index_node(mml::pointer_index_node *const node, int lvl) {
  ASSERT_UNSPEC;
  // TODO: implement this
  throw "not implemented";
}

void mml::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_UNSPEC;
  try {
    node->lvalue()->accept(this, lvl);
    node->type(node->lvalue()->type());
  } catch (const std::string &id) {
    throw "undeclared variable '" + id + "'";
  }
}

void mml::type_checker::do_assignment_node(cdk::assignment_node *const node, int lvl) {
  ASSERT_UNSPEC;

  try {
    node->lvalue()->accept(this, lvl);
  } catch (const std::string &id) {
    auto symbol = std::make_shared<mml::symbol>(cdk::primitive_type::create(4, cdk::TYPE_INT), id, 0);
    _symtab.insert(id, symbol);
    _parent->set_new_symbol(symbol);  // advise parent that a symbol has been inserted
    node->lvalue()->accept(this, lvl);  // DAVID: bah!
  }

  if (!node->lvalue()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in left argument of assignment expression");

  node->rvalue()->accept(this, lvl + 2);
  if (!node->rvalue()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type in right argument of assignment expression");

  // in MML, expressions are always int
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

//---------------------------------------------------------------------------

void mml::type_checker::do_function_node(mml::function_node *const node, int lvl) {
  // TODO: ensure node->arguments() are `declaration_node`s
}

//---------------------------------------------------------------------------

void mml::type_checker::do_evaluation_node(mml::evaluation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
}

void mml::type_checker::do_return_node(mml::return_node *const node, int lvl) {
  // TODO: implement this
  throw "not implemented";
}

void mml::type_checker::do_print_node(mml::print_node *const node, int lvl) {
  node->arguments()->accept(this, lvl + 2);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_input_node(mml::input_node *const node, int lvl) {
    // TODO: implement this
  throw "not implemented";
}

//---------------------------------------------------------------------------

void mml::type_checker::do_while_node(mml::while_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_if_node(mml::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
}

void mml::type_checker::do_if_else_node(mml::if_else_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
}

//---------------------------------------------------------------------------

void mml::type_checker::do_declaration_node(mml::declaration_node *const node, int lvl) {
  // TODO: implement this
  throw "not implemented";
}

//---------------------------------------------------------------------------

void mml::type_checker::do_function_call_node(mml::function_call_node *const node, int lvl) {
  // TODO: implement this
  throw "not implemented";
}

//---------------------------------------------------------------------------

void mml::type_checker::do_block_node(mml::block_node *const node, int lvl) {
  // TODO: implement this
  throw "not implemented";
}

//---------------------------------------------------------------------------

void mml::type_checker::do_nullptr_node(mml::nullptr_node *const node, int lvl) {
  // TODO: implement this
  throw "not implemented";
}

//---------------------------------------------------------------------------

void mml::type_checker::do_next_node(mml::next_node *const node, int lvl) {
  // TODO: implement this
  throw "not implemented";
}

void mml::type_checker::do_stop_node(mml::stop_node *const node, int lvl) {
  // TODO: implement this
  throw "not implemented";
}

//---------------------------------------------------------------------------

void mml::type_checker::do_sizeof_node(mml::sizeof_node *const node, int lvl) {
  // TODO: implement this
  throw "not implemented";
}
