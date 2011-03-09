#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <assert.h>
#include <string.h>
#include <syck.h>

#define EMITTER_VAL(v) (*((SyckEmitter **) Data_custom_val(v)))

static void
emitter_finalize(value parser)
{
    SyckEmitter *e = EMITTER_VAL(parser);
    syck_free_emitter(e);
}

static struct custom_operations emitter_ops = {
    "net.whytheluckystiff.syck.emitter",
    emitter_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

value yamlEmitter_create(value unit) 
{
  CAMLparam1(unit);
  SyckEmitter *em = syck_new_emitter();
  em->bonus = NULL;
  value em_v = caml_alloc_custom(&emitter_ops, sizeof(em), 0, 1);
  EMITTER_VAL(em_v) = em;
  CAMLreturn(em_v);
}

void caml_output_handler(SyckEmitter *e, const char *ptr, long len)
{
  CAMLparam0();
  CAMLlocal2(cb,buf);
  buf = caml_alloc_string(len);
  strncpy(String_val(buf),ptr,len);
  cb = (value)e->bonus;
  caml_callback(cb,buf);
  CAMLreturn0;
}

void caml_emitter_handler(SyckEmitter *e, st_data_t data)
{
  CAMLparam1(data);
  CAMLlocal3(node, lst, pair);
  char *node_uri, *node_str; 
  int str_len;

  node = (value)data;
  node_uri = String_val(Field(node,0));

  if (node_uri[0] != '\0')
    node_uri = syck_type_id_to_uri(node_uri);

  /* catch if the OCaml type changes in an unsafe way */
  assert(Is_block(node));

  switch(Tag_val(node)) {

  case 0: /* | SCALAR of uri * string*/
    node_str = String_val(Field(node,1));
    str_len = caml_string_length(Field(node,1));
    syck_emit_scalar(e,node_uri,scalar_plain,0,0,0,node_str,str_len);
    break;

  case 1: /* | SEQUENCE of uri * t list */
    syck_emit_seq(e, "seq", seq_none);
    lst = Field(node,1);
    while(lst != Val_unit) {
      syck_emit_item(e,(st_data_t)Field(lst,0));
      lst = Field(lst,1);
    }
    syck_emit_end(e);
    break;

  case 2: /* | MAPPING of uri * (t * t) list */
    syck_emit_map(e, "map", map_none);
    lst = Field(node,1);
    while(lst != Val_unit) {
      pair = Field(lst,0);
      syck_emit_item(e, (st_data_t)Field(pair,0));
      syck_emit_item(e, (st_data_t)Field(pair,1));
      lst = Field(lst,1);
    }
    syck_emit_end(e);
    break;
  default: /* OCaml type has a new variant */
    assert(0);
  }

  CAMLreturn0;
}

value yamlEmitter_emit(value handler, value emitter, value node)
{
  CAMLparam3(handler,emitter,node);

    SyckEmitter *em = EMITTER_VAL(emitter);
    
    em->bonus = (void*)handler;
    caml_register_global_root((value*)&(em->bonus));

    syck_output_handler(em,&caml_output_handler);
    syck_emitter_handler(em,&caml_emitter_handler);

    syck_emit(em, (st_data_t)node);
    syck_emitter_flush(em,0);

    caml_remove_global_root((value*)&(em->bonus));
    em->bonus = NULL;

    CAMLreturn(Val_unit);
}
