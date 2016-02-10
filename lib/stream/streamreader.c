/*------------------------------------------------------------------------------
 *    Copyright 2015 Chris Rink
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *----------------------------------------------------------------------------*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "streamreader.h"

enum StreamType {
    TYPE_STRING,
    TYPE_FILE
};

union StreamValue {
    FILE *f;
    const char *s;
};

struct ms_StreamReader {
    union StreamValue val;  /** value of the stream */
    enum StreamType type;   /** type of stream (STRING or FILE) */
    size_t pos;             /** current stream position */
    size_t len;             /** length of the stream (STRING only) */
    int cur;                /** current character */
};

ms_StreamReader *ms_StreamNewString(const char *str) {
    size_t len = strlen(str);
    return ms_StreamNewStringL(str, len);
}

ms_StreamReader *ms_StreamNewStringL(const char *str, size_t len) {
    ms_StreamReader *stream = malloc(sizeof(ms_StreamReader));
    if (!stream) {
        return NULL;
    }

    stream->type = TYPE_STRING;
    stream->val.s = str;
    stream->pos = 0;
    stream->len = len;
    stream->cur = EOF;
    return stream;
}

ms_StreamReader *ms_StreamNewFile(const char *fname) {
    ms_StreamReader *stream = malloc(sizeof(ms_StreamReader));
    if (!stream) {
        return NULL;
    }

    stream->val.f = fopen(fname, "r");
    if (!stream->val.f) {
        ms_StreamDestroy(stream);
        return NULL;
    }

    stream->type = TYPE_FILE;
    stream->pos = 0;
    stream->len = 0;
    stream->cur = EOF;
    return stream;
}

void ms_StreamDestroy(ms_StreamReader *stream) {
    if (!stream) { return; }
    if ((stream->type == TYPE_FILE) && (stream->val.f)) {
        int err = fclose(stream->val.f);
        if (err == EOF) {
            // TODO: something
        }
    }
    free(stream);
}

int ms_StreamNextChar(ms_StreamReader *stream) {
    if (!stream) {
        return EOF;
    }

    switch (stream->type) {
        case TYPE_FILE:
            stream->cur = fgetc(stream->val.f);
            stream->pos += (stream->cur != EOF) ? 1 : 0;
            return stream->cur;
        case TYPE_STRING:
            if (stream->pos < stream->len) {
                stream->cur = stream->val.s[stream->pos];
                stream->pos++;
                return stream->cur;
            } else {
                stream->cur = EOF;
                return EOF;
            }
        default:
            return EOF;
    }
}

int ms_StreamUnread(ms_StreamReader *stream) {
    if (!stream) {
        return EOF;
    }

    switch (stream->type) {
        case TYPE_FILE:
            if (stream->pos == 0) {
                return EOF;
            } else {
                long pos = stream->pos - 1;
                int res = fseek(stream->val.f, pos, SEEK_SET);
                if (res == 0) {
                    stream->pos--;
                    stream->cur = fgetc(stream->val.f);
                    ungetc(stream->cur, stream->val.f);
                    return stream->cur;
                }
                return EOF;
            }
        case TYPE_STRING:
            if (stream->pos == 0) {
                return EOF;
            } else if (stream->pos <= stream->len) {
                do {
                    stream->cur = stream->val.s[stream->pos];
                    stream->pos--;
                } while (stream->cur == '\0');
                return stream->cur;
            } else {
                stream->cur = EOF;
                return EOF;
            }
        default:
            return EOF;
    }
}
