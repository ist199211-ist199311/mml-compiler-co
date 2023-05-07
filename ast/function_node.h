#ifndef __MML_AST_FUNCTION_NODE_H__
#define __MML_AST_FUNCTION_NODE_H__

#include <cdk/ast/typed_node.h>

namespace mml {

  /**
   * Class for describing function nodes.
   */
  class function_node: public cdk::typed_node {
    cdk::sequence_node *_arguments;
    cdk::basic_node *_statements;
    bool _is_main;

  public:
    inline function_node(int lineno,
          cdk::sequence_node *arguments,
          cdk::basic_node *statements,
          bool is_main = false) :
        cdk::typed_node(lineno), _arguments(arguments), _statements(statements), _is_main(is_main) {
    }

  public:
    inline cdk::sequence_node *arguments() {
      return _arguments;
    }
    inline cdk::basic_node *statements() {
      return _statements;
    }
    inline bool is_main() {
      return _is_main;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_node(this, level);
    }

  };

} // mml

#endif
