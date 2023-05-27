#ifndef __MML_TARGETS_XML_WRITER_H__
#define __MML_TARGETS_XML_WRITER_H__

#include "targets/basic_ast_visitor.h"
#include <cdk/ast/basic_node.h>

namespace mml {

  /**
   * Print nodes as XML elements to the output stream.
   */
  class xml_writer: public basic_ast_visitor {
    cdk::symbol_table<mml::symbol> &_symtab;

  public:
    xml_writer(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<mml::symbol> &symtab) :
        basic_ast_visitor(compiler), _symtab(symtab) {
    }

  public:
    ~xml_writer() {
      os().flush();
    }

  private:
    void openTag(const std::string &tag, int lvl) {
      os() << std::string(lvl, ' ') + "<" + tag + ">" << std::endl;
    }
    void openTag(const cdk::basic_node *node, int lvl) {
      openTag(node->label(), lvl);
    }
    template<class... Attributes>
    void openTagWithAttributes(const std::string &tag, int lvl, Attributes&&... attrs) {
      os() << std::string(lvl, ' ') + "<" + tag;

      ((os() << " " << std::get<0>(attrs) << "=\"" << std::get<1>(attrs) << "\""), ...);

      os() << ">" << std::endl;
    }
    template<class... Attributes>
    void openTagWithAttributes(const cdk::basic_node *node, int lvl, Attributes&&... attrs) {
      openTagWithAttributes(node->label(), lvl, attrs...);
    }
    void closeTag(const std::string &tag, int lvl) {
      os() << std::string(lvl, ' ') + "</" + tag + ">" << std::endl;
    }
    void closeTag(const cdk::basic_node *node, int lvl) {
      closeTag(node->label(), lvl);
    }
    void emptyTag(const std::string &tag, int lvl) {
      os() << std::string(lvl, ' ') + "<" + tag + "/>" << std::endl;
    }
    void emptyTag(const cdk::basic_node *node, int lvl) {
      emptyTag(node->label(), lvl);
    }

  protected:
    void do_binary_operation(cdk::binary_operation_node *const node, int lvl);
    void do_unary_operation(cdk::unary_operation_node *const node, int lvl);
    inline const char* bool_to_str(bool boolean) {
      return boolean ? "true" : "false";
    }
    template<typename T>
    void process_literal(cdk::literal_node<T> *const node, int lvl) {
      os() << std::string(lvl, ' ') << "<" << node->label() << ">" << node->value() << "</" << node->label() << ">" << std::endl;
    }

  public:
    // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h"       // automatically generated
#undef __IN_VISITOR_HEADER__
    // do not edit these lines: end

  };

} // mml

#endif
