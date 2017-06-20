#ifndef SAJSON_WRAPPER_H
#define SAJSON_WRAPPER_H

#include <stddef.h>
#include <stdint.h>

struct sajson_document;

#ifdef __cplusplus
extern "C" {
#endif

struct sajson_document *sajson_parse_single_allocation(char *str, size_t length, size_t *buffer);
void sajson_free_document(struct sajson_document *doc);
int sajson_has_error(struct sajson_document *doc);
size_t sajson_get_error_line(struct sajson_document *doc);
size_t sajson_get_error_column(struct sajson_document *doc);
const char *sajson_get_error_message(struct sajson_document *doc);
uint8_t sajson_get_root_type(struct sajson_document *doc);
const size_t *sajson_get_root(struct sajson_document *doc);
const unsigned char *sajson_get_input(struct sajson_document *doc);

#ifdef __cplusplus
}
#endif

#endif
