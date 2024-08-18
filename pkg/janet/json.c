/*
* Copyright (c) 2022 Calvin Rose
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to
* deal in the Software without restriction, including without limitation the
* rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
* sell copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in
* all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
* IN THE SOFTWARE.
*/

#include <janet.h>
#include <stdlib.h>
#include <errno.h>

/*****************/
/* JSON Decoding */
/*****************/

#define JSON_KEYWORD_KEY 0x10000
#define JSON_NULL_TO_NIL 0x20000

/* Check if a character is whitespace */
static int white(uint8_t c) {
    return c == '\t' || c == '\n' || c == ' ' || c == '\r';
}

/* Skip whitespace */
static void skipwhite(const char **p) {
    const char *cp = *p;
    for (;;) {
        if (white(*cp))
            cp++;
        else
            break;
    }
    *p = cp;
}

/* Get a hex digit value */
static int hexdig(char dig) {
    if (dig >= '0' && dig <= '9')
        return dig - '0';
    if (dig >= 'a' && dig <= 'f')
        return 10 + dig - 'a';
    if (dig >= 'A' && dig <= 'F')
        return 10 + dig - 'A';
    return -1;
}

/* Convert integer to hex character */
static const char hex_digits[] = "0123456789ABCDEF";
#define tohex(x) (hex_digits[x])

/* Read the hex value for a unicode escape */
static const char *decode_utf16_escape(const char *p, uint32_t *outpoint) {
    if (!p[0] || !p[1] || !p[2] || !p[3])
        return "unexpected end of source";
    int d1 = hexdig(p[0]);
    int d2 = hexdig(p[1]);
    int d3 = hexdig(p[2]);
    int d4 = hexdig(p[3]);
    if (d1 < 0 || d2 < 0 || d3 < 0 || d4 < 0)
        return "invalid hex digit";
    *outpoint = d4 | (d3 << 4) | (d2 << 8) | (d1 << 12);
    return NULL;
}

/* Parse a string. Also handles the conversion of utf-16 to
 * utf-8. */
static const char *decode_string(const char **p, Janet *out) {
    JanetBuffer *buffer = janet_buffer(0);
    const char *cp = *p;
    while (*cp != '"') {
        uint8_t b = (uint8_t) *cp;
        if (b < 32) return "invalid character in string";
        if (b == '\\') {
            cp++;
            switch(*cp) {
                default:
                    return "unknown string escape";
                case 'b':
                    b = '\b';
                    break;
                case 'f':
                    b = '\f';
                    break;
                case 'n':
                    b = '\n';
                    break;
                case 'r':
                    b = '\r';
                    break;
                case 't':
                    b = '\t';
                    break;
                case '"':
                    b = '"';
                    break;
                case '\\':
                    b = '\\';
                    break;
                case '/':
                    b = '/';
                    break;
                case 'u':
                    {
                        /* Get codepoint and check for surrogate pair */
                        uint32_t codepoint;
                        const char *err = decode_utf16_escape(cp + 1, &codepoint);
                        if (err) return err;
                        if (codepoint >= 0xDC00 && codepoint <= 0xDFFF) {
                            return "unexpected utf-16 low surrogate";
                        } else if (codepoint >= 0xD800 && codepoint <= 0xDBFF) {
                            if (cp[5] != '\\') return "expected utf-16 low surrogate pair";
                            if (cp[6] != 'u') return "expected utf-16 low surrogate pair";
                            uint32_t lowsur;
                            const char *err = decode_utf16_escape(cp + 7, &lowsur);
                            if (err) return err;
                            if (lowsur < 0xDC00 || lowsur > 0xDFFF)
                                return "expected utf-16 low surrogate pair";
                            codepoint = ((codepoint - 0xD800) << 10) +
                                (lowsur - 0xDC00) + 0x10000;
                            cp += 11;
                        } else {
                            cp += 5;
                        }
                        /* Write codepoint */
                        if (codepoint <= 0x7F) {
                            janet_buffer_push_u8(buffer, codepoint);
                        } else if (codepoint <= 0x7FF) {
                            janet_buffer_push_u8(buffer, ((codepoint >>  6) & 0x1F) | 0xC0);
                            janet_buffer_push_u8(buffer, ((codepoint >>  0) & 0x3F) | 0x80);
                        } else if (codepoint <= 0xFFFF) {
                            janet_buffer_push_u8(buffer, ((codepoint >> 12) & 0x0F) | 0xE0);
                            janet_buffer_push_u8(buffer, ((codepoint >>  6) & 0x3F) | 0x80);
                            janet_buffer_push_u8(buffer, ((codepoint >>  0) & 0x3F) | 0x80);
                        } else {
                            janet_buffer_push_u8(buffer, ((codepoint >> 18) & 0x07) | 0xF0);
                            janet_buffer_push_u8(buffer, ((codepoint >> 12) & 0x3F) | 0x80);
                            janet_buffer_push_u8(buffer, ((codepoint >>  6) & 0x3F) | 0x80);
                            janet_buffer_push_u8(buffer, ((codepoint >>  0) & 0x3F) | 0x80);
                        }
                    }
                    continue;
            }
        }
        janet_buffer_push_u8(buffer, b);
        cp++;
    }
    *out = janet_stringv(buffer->data, buffer->count);
    *p = cp + 1;
    return NULL;
}

static const char *decode_one(const char **p, Janet *out, int depth) {

    /* Prevent stack overflow */
    if ((depth & 0xFFFF) > JANET_RECURSION_GUARD) goto recurdepth;

    /* Skip leading whitepspace */
    skipwhite(p);

    /* Main switch */
    switch (**p) {
        default:
            goto badchar;
        case '\0':
            goto eos;
        /* Numbers */
        case '-': case '0': case '1' : case '2': case '3' : case '4':
        case '5': case '6': case '7' : case '8': case '9':
            {
                errno = 0;
                char *end = NULL;
                double x = strtod(*p, &end);
                if (end == *p) goto badnum;
                *p = end;
                *out = janet_wrap_number(x);
                break;
            }
        /* false, null, true */
        case 'f':
            {
                const char *cp = *p;
                if (cp[1] != 'a' || cp[2] != 'l' || cp[3] != 's' || cp[4] != 'e')
                    goto badident;
                *out = janet_wrap_false();
                *p = cp + 5;
                break;
            }
        case 'n':
            {
                const char *cp = *p;

                if (cp[1] != 'u' || cp[2] != 'l' || cp[3] != 'l')
                    goto badident;
                if (depth & JSON_NULL_TO_NIL) {
                    *out = janet_wrap_nil();
                } else {
                    *out = janet_ckeywordv("null");
                }
                *p = cp + 4;
                break;
            }
        case 't':
            {
                const char *cp = *p;
                if (cp[1] != 'r' || cp[2] != 'u' || cp[3] != 'e')
                    goto badident;
                *out = janet_wrap_true();
                *p = cp + 4;
                break;
            }
        /* String */
        case '"':
            {
                const char *cp = *p + 1;
                const char *start = cp;
                while ((*cp >= 32 || *cp < 0) && *cp != '"' && *cp != '\\')
                    cp++;
                /* Only use a buffer for strings with escapes, else just copy
                 * memory from source */
                if (*cp == '\\') {
                    *p = *p + 1;
                    const char *err = decode_string(p, out);
                    if (err) return err;
                    break;
                }
                if (*cp != '"') goto badchar;
                *p = cp + 1;
                *out = janet_stringv((const uint8_t *)start, cp - start);
                break;
            }
        /* Array */
        case '[':
            {
                *p = *p + 1;
                JanetArray *array = janet_array(0);
                const char *err;
                Janet subval;
                skipwhite(p);
                while (**p != ']') {
                    err = decode_one(p, &subval, depth + 1);
                    if (err) return err;
                    janet_array_push(array, subval);
                    skipwhite(p);
                    if (**p == ']') break;
                    if (**p != ',') goto wantcomma;
                    *p = *p + 1;
                }
                *p = *p + 1;
                *out = janet_wrap_array(array);
            }
            break;
        /* Object */
        case '{':
            {
                *p = *p + 1;
                JanetTable *table = janet_table(0);
                const char *err;
                Janet subkey, subval;
                skipwhite(p);
                while (**p != '}') {
                    skipwhite(p);
                    if (**p != '"') goto wantstring;
                    err = decode_one(p, &subkey, depth + 1);
                    if (err) return err;
                    skipwhite(p);
                    if (**p != ':') goto wantcolon;
                    *p = *p + 1;
                    err = decode_one(p, &subval, depth + 1);
                    if (err) return err;
                    if (depth & JSON_KEYWORD_KEY) {
                        JanetString str = janet_unwrap_string(subkey);
                        subkey = janet_keywordv(str, janet_string_length(str));
                    }
                    janet_table_put(table, subkey, subval);
                    skipwhite(p);
                    if (**p == '}') break;
                    if (**p != ',') goto wantcomma;
                    *p = *p + 1;
                }
                *p = *p + 1;
                *out = janet_wrap_table(table);
                break;
            }
    }

    /* Good return */
    return NULL;

    /* Errors */
recurdepth:
    return "recursed too deeply";
eos:
    return "unexpected end of source";
badident:
    return "bad identifier";
badnum:
    return "bad number";
wantcomma:
    return "expected comma";
wantcolon:
    return "expected colon";
badchar:
    return "unexpected character";
wantstring:
    return "expected json string";
}

static Janet json_decode(int32_t argc, Janet *argv) {
    janet_arity(argc, 1, 3);
    Janet ret = janet_wrap_nil();
    const char *err;
    const char *start;
    const char *p;
    if (janet_checktype(argv[0], JANET_BUFFER)) {
        JanetBuffer *buffer = janet_unwrap_buffer(argv[0]);
        /* Ensure 0 padded */
        janet_buffer_push_u8(buffer, 0);
        buffer->count--;
        start = p = (const char *)buffer->data;
    } else {
        JanetByteView bytes = janet_getbytes(argv, 0);
        start = p = (const char *)bytes.bytes;
    }
    int flags = 0;
    if (argc > 1 && janet_truthy(argv[1])) flags |= JSON_KEYWORD_KEY;
    if (argc > 2 && janet_truthy(argv[2])) flags |= JSON_NULL_TO_NIL;
    err = decode_one(&p, &ret, flags);
    /* Check trailing values */
    if (!err) {
        skipwhite(&p);
        if (*p) err = "unexpected extra token";
    }
    if (err)
        janet_panicf("decode error at position %d: %s", p - start, err);
    return ret;
}

/*****************/
/* JSON Encoding */
/*****************/

typedef struct {
    JanetBuffer *buffer;
    int32_t indent;
    const uint8_t *tab;
    const uint8_t *newline;
    int32_t tablen;
    int32_t newlinelen;
} Encoder;

static void encode_newline(Encoder *e) {
    janet_buffer_push_bytes(e->buffer, e->newline, e->newlinelen);
    /* Skip loop if no tab string */
    if (!e->tablen) return;
    for (int32_t i = 0; i < e->indent; i++)
        janet_buffer_push_bytes(e->buffer, e->tab, e->tablen);
}

static const char *encode_one(Encoder *e, Janet x, int depth) {
    if ((depth & 0xFFFF) > JANET_RECURSION_GUARD) goto recurdepth;
    switch(janet_type(x)) {
        default:
            goto badtype;
        case JANET_NIL:
            janet_buffer_push_cstring(e->buffer, "null");
            break;
        case JANET_BOOLEAN:
            janet_buffer_push_cstring(e->buffer,
                    janet_unwrap_boolean(x) ? "true" : "false");
            break;
        case JANET_NUMBER:
            {
                char cbuf[25];
                sprintf(cbuf, "%.17g", janet_unwrap_number(x));
                janet_buffer_push_cstring(e->buffer, cbuf);
            }
            break;
        case JANET_STRING:
        case JANET_SYMBOL:
        case JANET_KEYWORD:
        case JANET_BUFFER:
            {
                const uint8_t *bytes;
                const uint8_t *c;
                const uint8_t *end;
                int32_t len;
                if (janet_keyeq(x, "null")) {
                    janet_buffer_push_cstring(e->buffer, "null");
                    break;
                }
                janet_bytes_view(x, &bytes, &len);
                janet_buffer_push_u8(e->buffer, '"');
                c = bytes;
                end = bytes + len;
                while (c < end) {

                    /* get codepoint */
                    uint32_t codepoint;
                    if (*c < 0x80) {
                        /* one byte */
                        codepoint = *c++;
                    } else if (*c < 0xE0) {
                        /* two bytes */
                        if (c + 2 > end) goto invalidutf8;
                        if ((c[1] >> 6) != 2) goto invalidutf8;
                        codepoint = ((c[0] & 0x1F) << 6) |
                            (c[1] & 0x3F);
                        c += 2;
                    } else if (*c < 0xF0) {
                        /* three bytes */
                        if (c + 3 > end) goto invalidutf8;
                        if ((c[1] >> 6) != 2) goto invalidutf8;
                        if ((c[2] >> 6) != 2) goto invalidutf8;
                        codepoint = ((c[0] & 0x0F) << 12) |
                            ((c[1] & 0x3F) << 6) |
                            (c[2] & 0x3F);
                        c += 3;
                    } else if (*c < 0xF8) {
                        /* four bytes */
                        if (c + 4 > end) goto invalidutf8;
                        if ((c[1] >> 6) != 2) goto invalidutf8;
                        if ((c[2] >> 6) != 2) goto invalidutf8;
                        if ((c[3] >> 6) != 2) goto invalidutf8;
                        codepoint = ((c[0] & 0x07) << 18) |
                            ((c[1] & 0x3F) << 12) |
                            ((c[2] & 0x3F) << 6) |
                            (c[3] & 0x3F);
                        c += 4;
                    } else {
                        /* invalid */
                        goto invalidutf8;
                    }

                    /* write codepoint */
                    if (codepoint > 0x1F && codepoint < 0x80) {
                        /* Normal, no escape */
                        if (codepoint == '\\' || codepoint == '"')
                            janet_buffer_push_u8(e->buffer, '\\');
                        janet_buffer_push_u8(e->buffer, (uint8_t) codepoint);
                    } else if (codepoint < 0x10000) {
                        /* One unicode escape */
                        uint8_t buf[6];
                        buf[0] = '\\';
                        buf[1] = 'u';
                        buf[2] = tohex((codepoint >> 12) & 0xF);
                        buf[3] = tohex((codepoint >> 8) & 0xF);
                        buf[4] = tohex((codepoint >> 4) & 0xF);
                        buf[5] = tohex(codepoint & 0xF);
                        janet_buffer_push_bytes(e->buffer, buf, sizeof(buf));
                    } else {
                        /* Two unicode escapes (surrogate pair) */
                        uint32_t hi, lo;
                        uint8_t buf[12];
                        hi = ((codepoint - 0x10000) >> 10) + 0xD800;
                        lo = ((codepoint - 0x10000) & 0x3FF) + 0xDC00;
                        buf[0] = '\\';
                        buf[1] = 'u';
                        buf[2] = tohex((hi >> 12) & 0xF);
                        buf[3] = tohex((hi >> 8) & 0xF);
                        buf[4] = tohex((hi >> 4) & 0xF);
                        buf[5] = tohex(hi & 0xF);
                        buf[6] = '\\';
                        buf[7] = 'u';
                        buf[8] = tohex((lo >> 12) & 0xF);
                        buf[9] = tohex((lo >> 8) & 0xF);
                        buf[10] = tohex((lo >> 4) & 0xF);
                        buf[11] = tohex(lo & 0xF);
                        janet_buffer_push_bytes(e->buffer, buf, sizeof(buf));
                    }
                }
                janet_buffer_push_u8(e->buffer, '"');
            }
            break;
        case JANET_TUPLE:
        case JANET_ARRAY:
            {
                const char *err;
                const Janet *items;
                int32_t len;
                janet_indexed_view(x, &items, &len);
                janet_buffer_push_u8(e->buffer, '[');
                e->indent++;
                for (int32_t i = 0; i < len; i++) {
                    encode_newline(e);
                    if ((err = encode_one(e, items[i], depth + 1))) return err;
                    janet_buffer_push_u8(e->buffer, ',');
                }
                e->indent--;
                if (e->buffer->data[e->buffer->count - 1] == ',') {
                    e->buffer->count--;
                    encode_newline(e);
                }
                janet_buffer_push_u8(e->buffer, ']');
            }
            break;
        case JANET_TABLE:
        case JANET_STRUCT:
            {
                const char *err;
                const JanetKV *kvs;
                int32_t count, capacity;
                janet_dictionary_view(x, &kvs, &count, &capacity);
                janet_buffer_push_u8(e->buffer, '{');
                e->indent++;
                for (int32_t i = 0; i < capacity; i++) {
                    if (janet_checktype(kvs[i].key, JANET_NIL))
                        continue;
                    if (!janet_checktypes(kvs[i].key, JANET_TFLAG_BYTES))
                        return "object key must be a byte sequence";
                    encode_newline(e);
                    if ((err = encode_one(e, kvs[i].key, depth + 1)))
                        return err;
                    const char *sep = e->tablen ? ": " : ":";
                    janet_buffer_push_cstring(e->buffer, sep);
                    if ((err = encode_one(e, kvs[i].value, depth + 1)))
                        return err;
                    janet_buffer_push_u8(e->buffer, ',');
                }
                e->indent--;
                if (e->buffer->data[e->buffer->count - 1] == ',') {
                    e->buffer->count--;
                    encode_newline(e);
                }
                janet_buffer_push_u8(e->buffer, '}');
            }
            break;
    }
    return NULL;

    /* Errors */

badtype:
    return "type not supported";
invalidutf8:
    return "string contains invalid utf-8";
recurdepth:
    return "recursed too deeply";
}

static Janet json_encode(int32_t argc, Janet *argv) {
    janet_arity(argc, 1, 4);
    Encoder e;
    e.indent = 0;
    e.buffer = janet_optbuffer(argv, argc, 3, 10);
    e.tab = NULL;
    e.newline = NULL;
    e.tablen = 0;
    e.newlinelen = 0;
    if (argc >= 2) {
        JanetByteView tab = janet_getbytes(argv, 1);
        e.tab = tab.bytes;
        e.tablen = tab.len;
        if (argc >= 3) {
            JanetByteView newline = janet_getbytes(argv, 2);
            e.newline = newline.bytes;
            e.newlinelen = newline.len;
        } else {
            e.newline = (const uint8_t *)"\r\n";
            e.newlinelen = 2;
        }
    }
    const char *err = encode_one(&e, argv[0], 0);
    if (err) janet_panicf("encode error: %s", err);
    return janet_wrap_buffer(e.buffer);
}

/****************/
/* Module Entry */
/****************/

static const JanetReg cfuns[] = {
    {"json/encode", json_encode,
        "(json/encode x &opt tab newline buf)\n\n"
        "Encodes a janet value in JSON (utf-8). tab and newline are optional byte sequence which are used "
        "to format the output JSON. if buf is provided, the formated JSON is append to buf instead of a new buffer. "
        "Returns the modifed buffer."
    },
    {"json/decode", json_decode,
        "(json/decode json-source &opt keywords nils)\n\n"
        "Returns a janet object after parsing JSON. If keywords is truthy, string "
        "keys will be converted to keywords. If nils is truthy, null will become nil instead "
        "of the keyword :null."
    },
    {NULL, NULL, NULL}
};

void module_json(JanetTable *env) {
    janet_cfuns(env, "json", cfuns);
}
