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
    bool _defined;
    int _offset = 0; // 0 means global
    long _value; // hack!

  public:
    symbol(const std::string &name, std::shared_ptr<cdk::basic_type> type, int qualifier, bool defined) :
        _name(name), _type(type), _qualifier(qualifier), _defined(defined), _value(0) {
    }

    virtual ~symbol() {
      // EMPTY
    }

    std::shared_ptr<cdk::basic_type> type() const {
      return _type;
    }
    bool is_typed(cdk::typename_type name) const {
      return _type->name() == name;
    }
    const std::string &name() const {
      return _name;
    }
    int qualifier() const {
      return _qualifier;
    }
    bool defined() const {
      return _defined;
    }
    void defined(bool defined) {
      _defined = defined;
    }
    long value() const {
      return _value;
    }
    long value(long v) {
      return _value = v;
    }

    inline auto make_symbol(const std::string &name, std::shared_ptr<cdk::basic_type> type, int qualifier, bool defined) {
      return std::make_shared<symbol>(name, type, qualifier, defined);
    }
  };

} // mml

#endif
