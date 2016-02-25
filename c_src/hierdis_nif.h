#ifndef HIERDIS_H
#define HIERDIS_H

#include <stdbool.h>
#include <string.h>
#include "erl_nif.h"
#include "hiredis.h"
#include "sds.h"

static ErlNifResourceType* HIREDIS_CONTEXT_RESOURCE;
typedef struct
{
    redisContext *context;
} hiredis_context_handle;

static ErlNifResourceType* HIREDIS_REPLY_RESOURCE;
typedef struct
{
    redisReply *reply;
} hiredis_reply_handle;

ERL_NIF_TERM ATOM_OK;
ERL_NIF_TERM ATOM_ERROR;
ERL_NIF_TERM ATOM_BADARG;
ERL_NIF_TERM ATOM_TRUE;
ERL_NIF_TERM ATOM_FALSE;
ERL_NIF_TERM ATOM_REDIS_ERR_IO;
ERL_NIF_TERM ATOM_REDIS_ERR_EOF;
ERL_NIF_TERM ATOM_REDIS_ERR_PROTOCOL;
ERL_NIF_TERM ATOM_REDIS_ERR_OOM;
ERL_NIF_TERM ATOM_REDIS_ERR_OTHER;
ERL_NIF_TERM ATOM_REDIS_REPLY_ERROR;

static ERL_NIF_TERM hierdis_make_error(ErlNifEnv* env, int code, const char* reason);
static ERL_NIF_TERM hierdis_make_binary_from_reply(ErlNifEnv* env, redisReply* r);
static ERL_NIF_TERM hierdis_make_list_from_reply(ErlNifEnv* env, redisReply* r);
static ERL_NIF_TERM hierdis_make_response(ErlNifEnv* env, redisReply* r, bool as_copy);
static void list_to_hiredis_argv(ErlNifEnv* env, ERL_NIF_TERM list, unsigned int argc, const char* argv[], size_t argv_lengths[]);

static ERL_NIF_TERM connect(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);;
static ERL_NIF_TERM connect_unix(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM command(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ERL_NIF_TERM append_command(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM get_reply(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

void hierdis_free_reply(void *reply);
void hiredis_context_handle_dtor(ErlNifEnv* env, void* arg);
void hiredis_reply_handle_dtor(ErlNifEnv* env, void* arg);

static int on_nif_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);

#endif
