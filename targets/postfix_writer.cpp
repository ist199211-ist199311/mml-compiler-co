#include <string>
#include <sstream>
#include "targets/type_checker.h"
#include "targets/postfix_writer.h"
#include "targets/frame_size_calculator.h"
#include ".auto/all_nodes.h"  // all_nodes.h is automatically generated

#include "mml_parser.tab.h"

//---------------------------------------------------------------------------

void mml::postfix_writer::do_nil_node(cdk::nil_node * const node, int lvl) {
  // EMPTY
}
void mml::postfix_writer::do_data_node(cdk::data_node * const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_sequence_node(cdk::sequence_node * const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_integer_node(cdk::integer_node * const node, int lvl) {
  if (inFunction()) {
    _pf.INT(node->value()); // push an integer
  } else {
    _pf.SINT(node->value());
  }
}

void mml::postfix_writer::do_double_node(cdk::double_node * const node, int lvl) {
  if (inFunction()) {
    _pf.DOUBLE(node->value());
  } else {
    _pf.SDOUBLE(node->value());
  }
}

void mml::postfix_writer::do_string_node(cdk::string_node * const node, int lvl) {
  int lbl1;

  /* generate the string */
  _pf.RODATA(); // strings are DATA readonly
  _pf.ALIGN(); // make sure we are aligned
  _pf.LABEL(mklbl(lbl1 = ++_lbl)); // give the string a name
  _pf.SSTRING(node->value()); // output string characters

  if (inFunction()) {
    /* leave the address on the stack */
    _pf.TEXT(_functionLabels.top()); // return to the TEXT segment
    _pf.ADDR(mklbl(lbl1)); // the string to be stored
  } else {
    _pf.DATA(); // return to the DATA segment
    _pf.SADDR(mklbl(lbl1)); // the string to be stored
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_neg_node(cdk::neg_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DNEG();
  } else {
    _pf.NEG();
  }
}

void mml::postfix_writer::do_identity_node(mml::identity_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
}

void mml::postfix_writer::do_not_node(cdk::not_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->argument()->accept(this, lvl + 2);
  _pf.INT(0);
  _pf.EQ();
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_add_node(cdk::add_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->left()->is_typed(cdk::TYPE_INT)) {
    auto ref = cdk::reference_type::cast(node->type());
    _pf.INT(std::max((size_t) 1, ref->referenced()->size()));
    _pf.MUL();
  }

  node->right()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    auto ref = cdk::reference_type::cast(node->type());
    _pf.INT(std::max((size_t) 1, ref->referenced()->size()));
    _pf.MUL();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DADD();
  } else {
    _pf.ADD();
  }
}
void mml::postfix_writer::do_sub_node(cdk::sub_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->left()->is_typed(cdk::TYPE_INT)) {
    auto ref = cdk::reference_type::cast(node->type());
    _pf.INT(std::max((size_t) 1, ref->referenced()->size()));
    _pf.MUL();
  }

  node->right()->accept(this, lvl);
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  } else if (node->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)) {
    auto ref = cdk::reference_type::cast(node->type());
    _pf.INT(std::max((size_t) 1, ref->referenced()->size()));
    _pf.MUL();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DSUB();
  } else {
    _pf.SUB();
  }

  if (node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_POINTER)) {
    // the difference between two pointers must be divided by the size of what they're referencing
    auto lref = cdk::reference_type::cast(node->left()->type());
    _pf.INT(std::max((size_t) 1, lref->referenced()->size()));
    _pf.DIV();
  }
}

void mml::postfix_writer::prepareIDBinaryExpression(cdk::binary_operation_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl);
  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl);
  if (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }
}

void mml::postfix_writer::do_mul_node(cdk::mul_node * const node, int lvl) {
  prepareIDBinaryExpression(node, lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DMUL();
  } else {
    _pf.MUL();
  }
}
void mml::postfix_writer::do_div_node(cdk::div_node * const node, int lvl) {
  prepareIDBinaryExpression(node, lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DDIV();
  } else {
    _pf.DIV();
  }
}
void mml::postfix_writer::do_mod_node(cdk::mod_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.MOD();
}

void mml::postfix_writer::prepareIDBinaryComparisonExpression(cdk::binary_operation_node * const node, int lvl) {
  prepareIDBinaryExpression(node, lvl);

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }
}

void mml::postfix_writer::do_lt_node(cdk::lt_node * const node, int lvl) {
  prepareIDBinaryComparisonExpression(node, lvl);
  _pf.LT();
}
void mml::postfix_writer::do_le_node(cdk::le_node * const node, int lvl) {
  prepareIDBinaryComparisonExpression(node, lvl);
  _pf.LE();
}
void mml::postfix_writer::do_ge_node(cdk::ge_node * const node, int lvl) {
  prepareIDBinaryComparisonExpression(node, lvl);
  _pf.GE();
}
void mml::postfix_writer::do_gt_node(cdk::gt_node * const node, int lvl) {
  prepareIDBinaryComparisonExpression(node, lvl);
  _pf.GT();
}
void mml::postfix_writer::do_ne_node(cdk::ne_node * const node, int lvl) {
  prepareIDBinaryComparisonExpression(node, lvl);
  _pf.NE();
}
void mml::postfix_writer::do_eq_node(cdk::eq_node * const node, int lvl) {
  prepareIDBinaryComparisonExpression(node, lvl);
  _pf.EQ();
}

void mml::postfix_writer::do_and_node(cdk::and_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  int lbl;
  node->left()->accept(this, lvl);
  _pf.DUP32();
  _pf.JZ(mklbl(lbl = ++_lbl)); // short circuit
  node->right()->accept(this, lvl);
  _pf.AND();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl));
}
void mml::postfix_writer::do_or_node(cdk::or_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  int lbl;
  node->left()->accept(this, lvl);
  _pf.DUP32();
  _pf.JNZ(mklbl(lbl = ++_lbl)); // short circuit
  node->right()->accept(this, lvl);
  _pf.OR();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl));
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_address_of_node(mml::address_of_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl + 2);
}

void mml::postfix_writer::do_alloc_node(mml::alloc_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  auto ref = cdk::reference_type::cast(node->type())->referenced();
  node->argument()->accept(this, lvl);
  // void has size 0, but we want to alloc 1 byte for it
  _pf.INT(std::max((size_t) 1, ref->size()));
  _pf.MUL();
  _pf.ALLOC();
  _pf.SP();
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_variable_node(cdk::variable_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  auto symbol = _symtab.find(node->name()); // type checker already ensured symbol exists

  if (symbol->qualifier() == tFOREIGN) {
    _externalFunctionName = symbol->name();
  } else if (symbol->global()) {
    _pf.ADDR(node->name());
  } else {
    _pf.LOCAL(symbol->offset());
  }
}

void mml::postfix_writer::do_pointer_index_node(mml::pointer_index_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->base()->accept(this, lvl + 2);
  node->index()->accept(this, lvl + 2);
  _pf.INT(node->type()->size());
  _pf.MUL();
  _pf.ADD();
}

void mml::postfix_writer::do_rvalue_node(cdk::rvalue_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl);

  if (_externalFunctionName) {
    return; // name passed through this field; nothing in stack to be loaded
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.LDDOUBLE();
  } else {
    _pf.LDINT(); // non-ints are int-sized too
  }
}

void mml::postfix_writer::do_assignment_node(cdk::assignment_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->rvalue()->accept(this, lvl); // determine the new value
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    if (node->rvalue()->is_typed(cdk::TYPE_INT)) {
      _pf.I2D();
    }
    _pf.DUP64();
  } else {
    _pf.DUP32();
  }

  node->lvalue()->accept(this, lvl); // where to store the value
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.STDOUBLE(); // store the value at address
  } else {
    _pf.STINT(); // store the value at address
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_function_node(mml::function_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  std::string functionLabel;
  if (node->is_main()) {
    functionLabel = "_main";
  } else {
    functionLabel = mklbl(++_lbl);
  }
  _functionLabels.push(functionLabel);

  _pf.TEXT(_functionLabels.top());
  _pf.ALIGN();
  if (node->is_main()) {
    _pf.GLOBAL("_main", _pf.FUNC());
  }
  _pf.LABEL(_functionLabels.top());

  auto oldOffset = _offset;
  _offset = 8; // function arguments start at offset 8
  _symtab.push();

  _inFunctionArgs = true;
  node->arguments()->accept(this, lvl);
  _inFunctionArgs = false;

  // compute stack size to be reserved for local variables
  frame_size_calculator fsc(_compiler, _symtab);
  node->block()->accept(&fsc, lvl);
  _pf.ENTER(fsc.localsize());

  auto oldFunctionRetLabel = _currentFunctionRetLabel;
  _currentFunctionRetLabel = mklbl(++_lbl);

  auto oldFunctionLoopLabels = _currentFunctionLoopLabels;
  _currentFunctionLoopLabels = new std::vector<std::pair<std::string, std::string>>();

  _offset = 0; // local variables start at offset 0

  node->block()->accept(this, lvl);

  if (node->is_main()) {
    // TODO: can this be refactored?
    // return 0 if main has no return statement
    _pf.INT(0);
    _pf.STFVAL32();
  }

  _pf.ALIGN();
  _pf.LABEL(_currentFunctionRetLabel);
  _pf.LEAVE();
  _pf.RET();

  delete _currentFunctionLoopLabels;
  _currentFunctionLoopLabels = oldFunctionLoopLabels;
  _currentFunctionRetLabel = oldFunctionRetLabel;
  _offset = oldOffset;
  _symtab.pop();
  _functionLabels.pop();

  if (node->is_main()) {
    // TODO: dynamically calculate this?
    _pf.EXTERN("readi");
    _pf.EXTERN("readd");
    _pf.EXTERN("printi");
    _pf.EXTERN("printd");
    _pf.EXTERN("prints");
    _pf.EXTERN("println");
    return;
  }

  // Since a function is also an expression, we need to push its address to the stack.
  // We should return to a text segment if this function is a local variable of another function
  // or to the data segment if it is global variable (except for the main function, which is not an expression).
  if (inFunction()) {
    _pf.TEXT(_functionLabels.top());
    _pf.ADDR(functionLabel);
  } else {
    _pf.DATA();
    _pf.SADDR(functionLabel);
  }
}

void mml::postfix_writer::do_return_node(mml::return_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  // symbol is validated in type checker, we are sure it exists
  auto symbol = _symtab.find("@", 1);
  auto rettype = cdk::functional_type::cast(symbol->type())->output(0);
  auto rettype_name = rettype->name();

  if (rettype_name != cdk::TYPE_VOID) {
    node->retval()->accept(this, lvl + 2);

    if (rettype_name == cdk::TYPE_DOUBLE) {
      if (node->retval()->is_typed(cdk::TYPE_INT)) {
        _pf.I2D();
      }
      _pf.STFVAL64();
    } else {
      _pf.STFVAL32();
    }
  }

  _pf.JMP(_currentFunctionRetLabel);
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_evaluation_node(mml::evaluation_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->argument()->accept(this, lvl);

  if (node->argument()->type()->size() > 0) {
    _pf.TRASH(node->argument()->type()->size());
  }
}

void mml::postfix_writer::do_print_node(mml::print_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  for (size_t ix = 0; ix < node->arguments()->size(); ix++) {
    auto child = dynamic_cast<cdk::expression_node*>(node->arguments()->node(ix));

    child->accept(this, lvl); // expression to print
    if (child->is_typed(cdk::TYPE_INT)) {
      _pf.CALL("printi");
      _pf.TRASH(4); // delete the printed value
    } else if (child->is_typed(cdk::TYPE_DOUBLE)) {
      _pf.CALL("printd");
      _pf.TRASH(8); // delete the printed value
    } else if (child->is_typed(cdk::TYPE_STRING)) {
      _pf.CALL("prints");
      _pf.TRASH(4); // delete the printed value's address
    }
  }

  if (node->append_newline()) {
    _pf.CALL("println");
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_input_node(mml::input_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.CALL("readd");
    _pf.LDFVAL64();
  } else {
    _pf.CALL("readi");
    _pf.LDFVAL32();
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_while_node(mml::while_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  int condLabel, endLabel;

  _pf.LABEL(mklbl(condLabel = ++_lbl));
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(endLabel = ++_lbl));

  _currentFunctionLoopLabels->push_back(std::make_pair(mklbl(condLabel), mklbl(endLabel)));
  node->block()->accept(this, lvl + 2);
  _currentFunctionLoopLabels->pop_back();

  _pf.JMP(mklbl(condLabel));
  _pf.LABEL(mklbl(endLabel));
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_if_node(mml::if_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->block()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1));
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_if_else_node(mml::if_else_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1, lbl2;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->thenblock()->accept(this, lvl + 2);
  _pf.JMP(mklbl(lbl2 = ++_lbl));
  _pf.LABEL(mklbl(lbl1));
  node->elseblock()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1 = lbl2));
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_declaration_node(mml::declaration_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  auto symbol = new_symbol();
  reset_new_symbol();

  int offset = 0;
  int typesize = node->type()->size(); // in bytes
  if (_inFunctionArgs) {
    offset = _offset;
    _offset += typesize;
  } else if (inFunction()) {
    _offset -= typesize;
    offset = _offset;
  } else {
    // global variable
    offset = 0;
  }
  symbol->offset(offset);

  // function local variables have to be handled separately
  if (inFunction()) {
    // nothing to do for function args or local variables without initializer
    if (_inFunctionArgs || node->initializer() == nullptr) {
      return;
    }

    node->initializer()->accept(this, lvl);
    if (node->is_typed(cdk::TYPE_DOUBLE)) {
      if (node->initializer()->is_typed(cdk::TYPE_INT)) {
        _pf.I2D();
      }
      _pf.LOCAL(symbol->offset());
      _pf.STDOUBLE();
    } else {
      _pf.LOCAL(symbol->offset());
      _pf.STINT();
    }

    return;
  }

  if (symbol->qualifier() == tFORWARD) {
      return; // nothing to do
  } else if (symbol->qualifier() == tFOREIGN) {
      _pf.EXTERN(symbol->name());
      return;
  }

  if (node->initializer() == nullptr) {
    _pf.BSS();
    _pf.ALIGN();

    if (symbol->qualifier() == tPUBLIC) {
      _pf.GLOBAL(symbol->name(), _pf.OBJ());
    }

    _pf.LABEL(symbol->name());
    _pf.SALLOC(typesize);
    return;
  }

  _pf.DATA();
  _pf.ALIGN();

  if (symbol->qualifier() == tPUBLIC) {
    _pf.GLOBAL(symbol->name(), _pf.OBJ());
  }

  _pf.LABEL(symbol->name());

  if (node->is_typed(cdk::TYPE_DOUBLE) && node->initializer()->is_typed(cdk::TYPE_INT)) {
    // The global declaration `double d = 1;` has to alloc a double and not an integer,
    // so we can't visit the integer_node.
    auto int_node = dynamic_cast<cdk::integer_node*>(node->initializer());
    _pf.SDOUBLE(int_node->value());
  } else {
    node->initializer()->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_function_call_node(mml::function_call_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  std::shared_ptr<cdk::functional_type> func_type;
  if (node->func() == nullptr) { // recursive call; "@"
    auto symbol = _symtab.find("@", 1);
    func_type = cdk::functional_type::cast(symbol->type());
  } else {
    func_type = cdk::functional_type::cast(node->func()->type());
  }

  int args_size = 0;
  // arguments must be visited in reverse order since the first argument has to be
  // on top of the stack
  for (size_t i = node->arguments()->size(); i > 0; i--) {
    auto arg = dynamic_cast<cdk::expression_node*>(node->arguments()->node(i - 1));

    args_size += arg->type()->size();
    arg->accept(this, lvl + 2);
    if (func_type->input(i - 1)->name() == cdk::TYPE_DOUBLE && arg->is_typed(cdk::TYPE_INT)) {
      _pf.I2D();
    }
  }

  _externalFunctionName = std::nullopt;
  if (node->func() == nullptr) { // recursive call; "@"
    _pf.ADDR(_functionLabels.top());
  } else {
    node->func()->accept(this, lvl);
  }

  if (_externalFunctionName) {
    _pf.CALL(*_externalFunctionName);
    _externalFunctionName = std::nullopt;
  } else {
    _pf.BRANCH();
  }

  if (args_size > 0) {
    _pf.TRASH(args_size);
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.LDFVAL64();
  } else if (!node->is_typed(cdk::TYPE_VOID)) {
    _pf.LDFVAL32();
  }
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_block_node(mml::block_node * const node, int lvl) {
  _symtab.push(); // for block-local variables
  node->declarations()->accept(this, lvl + 2);
  node->instructions()->accept(this, lvl + 2);
  _symtab.pop();
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_nullptr_node(mml::nullptr_node * const node, int lvl) {
  if (inFunction()) {
    _pf.INT(0);
  } else {
    _pf.SINT(0);
  }
}

//---------------------------------------------------------------------------

/** @tparam P index for loop labels pair */
template<size_t P, typename T>
void mml::postfix_writer::executeLoopControlInstruction(T * const node) {
  ASSERT_SAFE_EXPRESSIONS;

  if (node->level() == 0) {
    // TODO: extract this into a macro that outputs node->lineno() too
    std::cerr << "invalid loop control instruction level" << std::endl;
    exit(1);
  } else if (_currentFunctionLoopLabels->size() < node->level()) {
    // TODO: extract this into a macro that outputs node->lineno() too
    std::cerr << "loop control instruction not within sufficient loops" <<
        " (expected at most " << _currentFunctionLoopLabels->size() << ")" << std::endl;
    exit(1);
  }

  auto index = _currentFunctionLoopLabels->size() - node->level();
  auto label = std::get<P>(_currentFunctionLoopLabels->at(index));
  _pf.JMP(label);
}

void mml::postfix_writer::do_next_node(mml::next_node * const node, int lvl) {
  executeLoopControlInstruction<0>(node);
}

void mml::postfix_writer::do_stop_node(mml::stop_node * const node, int lvl) {
  executeLoopControlInstruction<1>(node);
}

//---------------------------------------------------------------------------

void mml::postfix_writer::do_sizeof_node(mml::sizeof_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  _pf.INT(node->argument()->type()->size());
}
