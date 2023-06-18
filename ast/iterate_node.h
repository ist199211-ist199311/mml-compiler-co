#ifndef __MML_AST_ITERATE_NODE_H__
#define __MML_AST_ITERATE_NODE_H__

#include <cdk/ast/expression_node.h>

namespace mml {

  /**
   * Class for describing iterate-cycle nodes.
   */
  class iterate_node: public cdk::basic_node {
    cdk::expression_node *_condition, *_vector, *_count, *_function;

  public:
    inline iterate_node(int lineno, cdk::expression_node *condition, cdk::expression_node *vector, cdk::expression_node *count, cdk::expression_node *function) :
        basic_node(lineno), _condition(condition), _vector(vector), _count(count), _function(function) {
    }

  public:
    inline cdk::expression_node *condition() {
      return _condition;
    }
    inline cdk::expression_node *vector() {
      return _vector;
    }
    inline cdk::expression_node *count() {
      return _count;
    }
    inline cdk::expression_node *function() {
      return _function;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_iterate_node(this, level);
    }

  };

} // mml

#endif
