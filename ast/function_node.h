#ifndef __MML_AST_FUNCTION_NODE_H__
#define __MML_AST_FUNCTION_NODE_H__

#include <cdk/ast/expression_node.h>
#include <cdk/ast/sequence_node.h>
#include "block_node.h"

namespace mml {

  /**
   * Class for describing function nodes.
   */
  class function_node: public cdk::expression_node {
    cdk::sequence_node *_arguments;
    mml::block_node *_block;
    bool _is_main;

  public:
    inline function_node(int lineno,
          cdk::sequence_node *arguments,
          mml::block_node *block,
          bool is_main = false) :
        cdk::expression_node(lineno), _arguments(arguments), _block(block), _is_main(is_main) {
    }
    /** Shorthand main function constructor. */
    inline function_node(int lineno, mml::block_node *block) :
        cdk::expression_node(lineno), _arguments(new cdk::sequence_node(lineno)), _is_main(true) {
    }

  public:
    inline cdk::sequence_node *arguments() {
      return _arguments;
    }
    inline cdk::basic_node *block() {
      return _block;
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
