#include "targets/frame_size_calculator.h"
#include "targets/type_checker.h"
#include ".auto/all_nodes.h"  // all_nodes.h is automatically generated

//---------------------------------------------------------------------------

void mml::frame_size_calculator::do_sequence_node(cdk::sequence_node * const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

void mml::frame_size_calculator::do_while_node(mml::while_node * const node, int lvl) {
  node->block()->accept(this, lvl);
}

void mml::frame_size_calculator::do_if_node(mml::if_node * const node, int lvl) {
  node->block()->accept(this, lvl);
}

void mml::frame_size_calculator::do_if_else_node(mml::if_else_node * const node, int lvl) {
  node->thenblock()->accept(this, lvl);
  node->elseblock()->accept(this, lvl);
}

void mml::frame_size_calculator::do_declaration_node(mml::declaration_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  _localsize += node->type()->size();
}

void mml::frame_size_calculator::do_block_node(mml::block_node * const node, int lvl) {
  _symtab.push();
  node->declarations()->accept(this, lvl);
  node->instructions()->accept(this, lvl);
  _symtab.pop();
}

//---------------------------------------------------------------------------

void mml::frame_size_calculator::do_nil_node(cdk::nil_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_data_node(cdk::data_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_integer_node(cdk::integer_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_double_node(cdk::double_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_string_node(cdk::string_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_neg_node(cdk::neg_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_identity_node(mml::identity_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_not_node(cdk::not_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_alloc_node(mml::alloc_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_add_node(cdk::add_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_sub_node(cdk::sub_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_mul_node(cdk::mul_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_div_node(cdk::div_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_mod_node(cdk::mod_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_lt_node(cdk::lt_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_le_node(cdk::le_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_ge_node(cdk::ge_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_gt_node(cdk::gt_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_ne_node(cdk::ne_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_eq_node(cdk::eq_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_and_node(cdk::and_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_or_node(cdk::or_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_address_of_node(mml::address_of_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_variable_node(cdk::variable_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_pointer_index_node(mml::pointer_index_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_rvalue_node(cdk::rvalue_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_assignment_node(cdk::assignment_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_function_node(mml::function_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_evaluation_node(mml::evaluation_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_return_node(mml::return_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_print_node(mml::print_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_input_node(mml::input_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_function_call_node(mml::function_call_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_nullptr_node(mml::nullptr_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_next_node(mml::next_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_stop_node(mml::stop_node * const node, int lvl) {
  // EMPTY
}
void mml::frame_size_calculator::do_sizeof_node(mml::sizeof_node * const node, int lvl) {
  // EMPTY
}
