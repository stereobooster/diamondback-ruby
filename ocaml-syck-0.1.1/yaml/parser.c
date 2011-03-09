/* -*- Mode: C; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/* Copyright (c) 2007 Christopher R. Waterson */

#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <syck.h>
#include <string.h>

struct node_list {
    struct node_list *next;
    value             value;
};

#define TAG_SCALAR   0
#define TAG_SEQUENCE 1
#define TAG_MAPPING  2

#define PARSER_VAL(v) (*((SyckParser **) Data_custom_val(v)))

static void
free_nodes(SyckParser *parser)
{
    struct node_list *nodes, *doomed;

    nodes = (struct node_list *) parser->bonus;
    while (nodes) {
        doomed = nodes;
        nodes = nodes->next;
        caml_remove_global_root(&doomed->value);
        free(doomed);
    }

    parser->bonus = 0;
}

static void
parser_finalize(value parser)
{
    SyckParser *p = PARSER_VAL(parser);
    free_nodes(p);
    syck_free_parser(p);
}

static struct custom_operations parser_ops = {
    "net.whytheluckystiff.syck.parser",
    parser_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

/* ---------------------------------------------------------------------- */

static void Noreturn
raise_error(const char *msg)
{
    caml_raise_with_string(*caml_named_value("YamlParser_Error"), msg);
}

static value
lookup_sym(SyckParser *parser, SYMID id)
{
    CAMLparam0();
    CAMLlocal1(sym);
    char *p;

    if (!syck_lookup_sym(parser, id, &p))
        raise_error("symbol lookup failed");

    sym = *((value *) p);

    CAMLreturn(sym);
}

static value
make_scalar(SyckParser *parser, SyckNode *node)
{
    CAMLparam0();
    CAMLlocal2(rv,buf);
    long len = node->data.str->len;

    buf = caml_alloc_string(len);
    strncpy(String_val(buf),node->data.str->ptr,len);

    rv = caml_alloc(2, TAG_SCALAR);
    Store_field(rv, 0, caml_copy_string(node->type_id ? node->type_id : ""));
    Store_field(rv, 1, buf);

    CAMLreturn(rv);
}

static value
make_sequence(SyckParser *parser, SyckNode *node)
{
    CAMLparam0();
    CAMLlocal3(list, cell, rv);
    int i;

    list = Val_int(0);
    for (i = node->data.list->idx - 1; i >= 0; --i) {
        SYMID id = syck_seq_read(node, i);

        cell = caml_alloc(2, 0);
        Store_field(cell, 0, lookup_sym(parser, id));
        Store_field(cell, 1, list);

        list = cell;
    }

    rv = caml_alloc(2, TAG_SEQUENCE);
    Store_field(rv, 0, caml_copy_string(node->type_id ? node->type_id : ""));
    Store_field(rv, 1, list);

    CAMLreturn(rv);
}

static value
make_mapping(SyckParser *parser, SyckNode *node)
{
    CAMLparam0();
    CAMLlocal4(mapping, cell, assoc, rv);
    int i;

    mapping = Val_int(0);
    for (i = node->data.pairs->idx - 1; i >= 0; --i) {
        SYMID key = syck_map_read(node, map_key, i);
        SYMID val = syck_map_read(node, map_value, i);

        assoc = caml_alloc(2, 0);
        Store_field(assoc, 0, lookup_sym(parser, key));
        Store_field(assoc, 1, lookup_sym(parser, val));

        cell = caml_alloc(2, 0);
        Store_field(cell, 0, assoc);
        Store_field(cell, 1, mapping);

        mapping = cell;
    }

    rv = caml_alloc(2, TAG_MAPPING);
    Store_field(rv, 0, caml_copy_string(node->type_id ? node->type_id : ""));
    Store_field(rv, 1, mapping);

    CAMLreturn(rv);
}

static SYMID
parser_handler(SyckParser *parser, SyckNode *node)
{
    CAMLparam0();
    CAMLlocal1(result);
    SYMID id;
    struct node_list *nodes;

    switch (node->kind) {
    case syck_str_kind:
        result = make_scalar(parser, node);
        break;

    case syck_seq_kind:
        result = make_sequence(parser, node);
        break;

    case syck_map_kind:
        result = make_mapping(parser, node);
        break;
    }

    /* Add the new thing we just made to the list of crap the parser
       has accrued.  This makes sure it doesn't get garbage
       collected. */
    nodes = malloc(sizeof *nodes);
    nodes->next = (struct node_list *) parser->bonus;
    nodes->value = result;
    caml_register_global_root(&nodes->value);

    parser->bonus = (void *) nodes;

    id = syck_add_sym(parser, (char *) &nodes->value);

    CAMLreturn(id);
}

static void Noreturn
error_handler(SyckParser *parser, const char *msg)
{
    raise_error(msg);
}

static SyckNode * Noreturn
bad_anchor_handler(SyckParser *parser, const char *anchor)
{
#if defined(HAVE_SNPRINTF)
    char buf[128];
    snprintf(buf, sizeof(buf), "bad anchor '%s'", anchor);
    raise_error(buf);
#else
    raise_error("bad anchor");
#endif
}

value
yamlParser_make(value unit)
{
    CAMLparam1(unit);
    CAMLlocal1(rv);

    SyckParser *parser = syck_new_parser();
    syck_parser_handler(parser, parser_handler);
    syck_parser_error_handler(parser, error_handler);
    syck_parser_bad_anchor_handler(parser, bad_anchor_handler);

    parser->bonus = (void *) 0;

    rv = caml_alloc_custom(&parser_ops, sizeof(parser), 0, 1);
    PARSER_VAL(rv) = parser;
    CAMLreturn(rv);
}

value
yamlParser_parse_string(value parser, value str)
{
    CAMLparam2(parser, str);
    CAMLlocal1(rv);
    SyckParser *p = PARSER_VAL(parser);
    SYMID v;
    int ok;
    char *valuep;

    /* Set the string to parse. */
    syck_parser_str(p, String_val(str),
                    caml_string_length(str), 0);

    /* Parse it. */
    v = syck_parse(p);

    /* Find the document root. */
    ok = syck_lookup_sym(p, v, &valuep);

    if (ok)
        rv = *((value *) valuep);

    free_nodes(p);

    if (!ok)
        raise_error("parse failed");

    CAMLreturn(rv);
}

