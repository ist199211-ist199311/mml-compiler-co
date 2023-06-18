#ifndef __MML_AST_ITERATE_NODE_H__
#define __MML_AST_ITERATE_NODE_H__

#include <cdk/ast/expression_node.h>

namespace mml {

  /**
   * Class for describing iterate nodes.
   */
  class iterate_node: public cdk::expression_node {
    cdk::expression_node *_vec, *_count, *_func, *_cond;

  public:
    inline iterate_node(int lineno, cdk::expression_node *vec,
            cdk::expression_node *count, cdk::expression_node *func,
            cdk::expression_node *cond) :
        cdk::expression_node(lineno), _vec(vec), _count(count), _func(func), _cond(cond) {
    }

  public:
    inline cdk::expression_node *vec() {
      return _vec;
    }
    inline cdk::expression_node *count() {
      return _count;
    }
    inline cdk::expression_node *func() {
      return _func;
    }
    inline cdk::expression_node *cond() {
      return _cond;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_iterate_node(this, level);
    }

  };

} // mml

#endif
