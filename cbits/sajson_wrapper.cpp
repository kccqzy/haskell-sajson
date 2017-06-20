#include "sajson_wrapper.h"
#include "sajson.hpp"
#include <new>
#include <stdlib.h>

// never instantiated, only inherits so static_cast is legal
struct sajson_document: sajson::document {};
struct sajson_value: sajson::value {};

static inline sajson::document* unwrap(sajson_document* doc) {
    return static_cast<sajson::document*>(doc);
}

static inline sajson_document* wrap(sajson::document* doc) {
    return static_cast<sajson_document*>(doc);
}

size_t sajson_document_sizeof(void) {
    return sizeof(sajson::document);
}

sajson_document* sajson_parse_single_allocation(char* str, size_t length, size_t *buffer, char *rv) {
    auto doc = sajson::parse(sajson::single_allocation(buffer, length), sajson::mutable_string_view(length, str));
    return wrap(new(rv) sajson::document(std::move(doc)));
}

void sajson_free_document(sajson_document* doc) {
    unwrap(doc)->~document();
}

int sajson_has_error(sajson_document* doc) {
    return !unwrap(doc)->is_valid();
}

size_t sajson_get_error_line(sajson_document* doc) {
    return unwrap(doc)->get_error_line();
}

size_t sajson_get_error_column(sajson_document* doc) {
    return unwrap(doc)->get_error_column();
}

const char* sajson_get_error_message(sajson_document* doc) {
    return unwrap(doc)->get_error_message_as_cstring();
}

uint8_t sajson_get_root_type(sajson_document* doc) {
    return unwrap(doc)->_internal_get_root_type();
}

const size_t* sajson_get_root(sajson_document* doc) {
    return unwrap(doc)->_internal_get_root();
}

const unsigned char* sajson_get_input(sajson_document* doc) {
    return reinterpret_cast<const unsigned char*>(
        unwrap(doc)->_internal_get_input().get_data());
}
