#ifndef __MML_TARGETS_POSTFIX_WRITER_H__
#define __MML_TARGETS_POSTFIX_WRITER_H__

#include "targets/basic_ast_visitor.h"

#include <optional>
#include <set>
#include <sstream>
#include <stack>
#include <cdk/emitters/basic_postfix_emitter.h>
#include <cdk/types/types.h>

namespace mml {

  //!
  //! Traverse syntax tree and generate the corresponding assembly code.
  //!
  class postfix_writer: public basic_ast_visitor {
    cdk::symbol_table<mml::symbol> &_symtab;

    bool _forceOutsideFunction = false; // whether to force future declarations to be global
    bool _inFunctionArgs = false;
    std::stack<std::string> _functionLabels; // (history of) label of current visiting function
    std::string _currentFunctionRetLabel; // where to jump when a return occurs
    int _offset; // current framepointer offset (0 means no vars defined)
    std::optional<std::string> _externalFunctionName; // name of external function to be called, if any
    std::set<std::string> _externalFunctionsToDeclare; // set of external functions to declare
    std::vector<std::pair<std::string, std::string>> *_currentFunctionLoopLabels;
    // ^ (history of) labels of current visiting function's loops; pair (condition, end)

    cdk::basic_postfix_emitter &_pf;
    int _lbl;

  public:
    postfix_writer(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<mml::symbol> &symtab,
                   cdk::basic_postfix_emitter &pf) :
        basic_ast_visitor(compiler), _symtab(symtab), _pf(pf), _lbl(0) {
    }

  public:
    ~postfix_writer() {
      os().flush();
    }

  protected:
    void prepareIDBinaryExpression(cdk::binary_operation_node * const node, int lvl);
    void prepareIDBinaryComparisonExpression(cdk::binary_operation_node * const node, int lvl);
    void acceptCovariantNode(std::shared_ptr<cdk::basic_type> const node_type, cdk::expression_node * const node, int lvl);
    template<size_t P, typename T> void executeLoopControlInstruction(T * const node);

  private:
    /** Method used to generate sequential labels. */
    inline std::string mklbl(int lbl) {
      std::ostringstream oss;
      if (lbl < 0)
        oss << ".L" << -lbl;
      else
        oss << "_L" << lbl;
      return oss.str();
    }

    inline bool inFunction() {
      return !_forceOutsideFunction && !_functionLabels.empty();
    }

    template<class T>
    inline bool isInstanceOf(cdk::basic_node * const node) {
      return dynamic_cast<T*>(node) != nullptr;
    }
    template<class T, class... Rest, typename std::enable_if<sizeof...(Rest) != 0, int>::type = 0>
    inline bool isInstanceOf(cdk::basic_node * const node) {
      return dynamic_cast<T*>(node) != nullptr || isInstanceOf<Rest...>(node);
    }

  public:
  // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h"       // automatically generated
#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end

  };

#define THROW_ERROR(msg) { \
  std::cerr << node->lineno() << ": " << msg << std::endl; \
  return; \
}

} // mml

#endif
