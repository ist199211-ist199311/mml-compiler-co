#ifndef __MML_AST_FUNCTION_CALL_NODE_H__
#define __MML_AST_FUNCTION_CALL_NODE_H__

#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>

namespace mml {

  /**
   * Class for describing function call nodes.
   */
  class function_call_node: public cdk::expression_node {
    cdk::expression_node *_func;
    cdk::sequence_node *_arguments;

  public:
    inline function_call_node(int lineno, cdk::expression_node *func, cdk::sequence_node *arguments) :
        cdk::expression_node(lineno), _func(func), _arguments(arguments) {
    }

  public:
    inline cdk::expression_node *func() {
      return _func;
    }
    inline cdk::sequence_node *arguments() {
      return _arguments;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_call_node(this, level);
    }

  };

} // mml

#endif
