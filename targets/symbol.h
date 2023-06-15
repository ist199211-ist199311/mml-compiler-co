#ifndef __MML_TARGETS_SYMBOL_H__
#define __MML_TARGETS_SYMBOL_H__

#include <string>
#include <memory>
#include <cdk/types/basic_type.h>

namespace mml {

  class symbol {
    std::string _name;
    std::shared_ptr<cdk::basic_type> _type;
    int _qualifier;
    int _offset = 0; // 0 means global
    bool _is_main = false; // whether this symbol is the @ relative to the main function

  public:
    symbol(const std::string &name, std::shared_ptr<cdk::basic_type> type, int qualifier) :
        _name(name), _type(type), _qualifier(qualifier) {
    }

    virtual ~symbol() {
      // EMPTY
    }

    const std::string &name() const {
      return _name;
    }
    std::shared_ptr<cdk::basic_type> type() const {
      return _type;
    }
    bool is_typed(cdk::typename_type name) const {
      return _type->name() == name;
    }
    int qualifier() const {
      return _qualifier;
    }
    int offset() const {
      return _offset;
    }
    int offset(int o) {
      return _offset = o;
    }
    bool global() const {
      return _offset == 0;
    }
    bool is_main() const {
      return _is_main;
    }
    bool is_main(bool b) {
      return _is_main = b;
    }
  };

  inline auto make_symbol(const std::string &name, std::shared_ptr<cdk::basic_type> type, int qualifier = 0) {
    return std::make_shared<symbol>(name, type, qualifier);
  }

} // mml

#endif
