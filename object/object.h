#ifndef OBJECT_H
#define OBJECT_H

#include <string>

namespace object {

enum class ObjectType {
    BOOLEAN,
    INTEGER,
    NULL_OBJ
};

class Object {
public:
    virtual ~Object() = default;

    virtual ObjectType Type() const = 0;
    virtual std::string Inspect() const = 0;
};

class Boolean : public Object {
public:
    explicit Boolean(bool value) : Value(value) {}

    ObjectType Type() const override { return ObjectType::BOOLEAN; }
    std::string Inspect() const override;

    bool Value;
};

class Integer : public Object {
public:
    explicit Integer(int64_t value) : Value(value) {}

    ObjectType Type() const override { return ObjectType::INTEGER; }
    std::string Inspect() const override;

    int64_t Value;
};

class Null : public Object {
public:
    ObjectType Type() const override { return ObjectType::NULL_OBJ; }
    std::string Inspect() const override;
};

}

#endif
