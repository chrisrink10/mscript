/*------------------------------------------------------------------------------
 *    Copyright 2016 Chris Rink
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

#include <assert.h>
#include <math.h>
#include "obj.h"
#include "vm.h"

/*
 * PROTOTYPE FUNCTIONS
 */

static int ms_FloatToStr(ms_VM *vm);
static int ms_FloatToFloat(ms_VM *vm);
static int ms_FloatToInt(ms_VM *vm);
static int ms_FloatToBool(ms_VM *vm);
static int ms_FloatAdd(ms_VM *vm);
static int ms_FloatSubtract(ms_VM *vm);
static int ms_FloatMultiply(ms_VM *vm);
static int ms_FloatDivide(ms_VM *vm);
static int ms_FloatIDivide(ms_VM *vm);
static int ms_FloatModulo(ms_VM *vm);
static int ms_FloatExponentiate(ms_VM *vm);
static int ms_FloatNegate(ms_VM *vm);
static int ms_FloatLessThan(ms_VM *vm);
static int ms_FloatLessEqual(ms_VM *vm);
static int ms_FloatGreaterThan(ms_VM *vm);
static int ms_FloatGreaterEqual(ms_VM *vm);
static int ms_FloatEqual(ms_VM *vm);
static int ms_FloatNotEqual(ms_VM *vm);
static int ms_FloatNot(ms_VM *vm);
static int ms_FloatAnd(ms_VM *vm);
static int ms_FloatOr(ms_VM *vm);

static int ms_IntToStr(ms_VM *vm);
static int ms_IntToFloat(ms_VM *vm);
static int ms_IntToInt(ms_VM *vm);
static int ms_IntToBool(ms_VM *vm);
static int ms_IntAdd(ms_VM *vm);
static int ms_IntSubtract(ms_VM *vm);
static int ms_IntMultiply(ms_VM *vm);
static int ms_IntDivide(ms_VM *vm);
static int ms_IntIDivide(ms_VM *vm);
static int ms_IntModulo(ms_VM *vm);
static int ms_IntExponentiate(ms_VM *vm);
static int ms_IntNegate(ms_VM *vm);
static int ms_IntLShift(ms_VM *vm);
static int ms_IntRShift(ms_VM *vm);
static int ms_IntBitwiseAnd(ms_VM *vm);
static int ms_IntBitwiseXor(ms_VM *vm);
static int ms_IntBitwiseOr(ms_VM *vm);
static int ms_IntBitwiseNot(ms_VM *vm);
static int ms_IntLessThan(ms_VM *vm);
static int ms_IntLessEqual(ms_VM *vm);
static int ms_IntGreaterThan(ms_VM *vm);
static int ms_IntGreaterEqual(ms_VM *vm);
static int ms_IntEqual(ms_VM *vm);
static int ms_IntNotEqual(ms_VM *vm);
static int ms_IntNot(ms_VM *vm);
static int ms_IntAnd(ms_VM *vm);
static int ms_IntOr(ms_VM *vm);

static int ms_StrToStr(ms_VM *vm);
static int ms_StrToFloat(ms_VM *vm);
static int ms_StrToInt(ms_VM *vm);
static int ms_StrToBool(ms_VM *vm);
static int ms_StrAdd(ms_VM *vm);
static int ms_StrLessThan(ms_VM *vm);
static int ms_StrLessEqual(ms_VM *vm);
static int ms_StrGreaterThan(ms_VM *vm);
static int ms_StrGreaterEqual(ms_VM *vm);
static int ms_StrEqual(ms_VM *vm);
static int ms_StrNotEqual(ms_VM *vm);
static int ms_StrNot(ms_VM *vm);
static int ms_StrAnd(ms_VM *vm);
static int ms_StrOr(ms_VM *vm);

static int ms_BoolToStr(ms_VM *vm);
static int ms_BoolToFloat(ms_VM *vm);
static int ms_BoolToInt(ms_VM *vm);
static int ms_BoolToBool(ms_VM *vm);
static int ms_BoolAdd(ms_VM *vm);
static int ms_BoolSubtract(ms_VM *vm);
static int ms_BoolMultiply(ms_VM *vm);
static int ms_BoolDivide(ms_VM *vm);
static int ms_BoolIDivide(ms_VM *vm);
static int ms_BoolModulo(ms_VM *vm);
static int ms_BoolExponentiate(ms_VM *vm);
static int ms_BoolNegate(ms_VM *vm);
static int ms_BoolLShift(ms_VM *vm);
static int ms_BoolRShift(ms_VM *vm);
static int ms_BoolBitwiseAnd(ms_VM *vm);
static int ms_BoolBitwiseXor(ms_VM *vm);
static int ms_BoolBitwiseOr(ms_VM *vm);
static int ms_BoolBitwiseNot(ms_VM *vm);
static int ms_BoolLessThan(ms_VM *vm);
static int ms_BoolLessEqual(ms_VM *vm);
static int ms_BoolGreaterThan(ms_VM *vm);
static int ms_BoolGreaterEqual(ms_VM *vm);
static int ms_BoolEqual(ms_VM *vm);
static int ms_BoolNotEqual(ms_VM *vm);
static int ms_BoolNot(ms_VM *vm);
static int ms_BoolAnd(ms_VM *vm);
static int ms_BoolOr(ms_VM *vm);

static int ms_NullToStr(ms_VM *vm);
static int ms_NullToFloat(ms_VM *vm);
static int ms_NullToInt(ms_VM *vm);
static int ms_NullToBool(ms_VM *vm);
static int ms_NullEqual(ms_VM *vm);
static int ms_NullNotEqual(ms_VM *vm);
static int ms_NullNot(ms_VM *vm);
static int ms_NullAnd(ms_VM *vm);
static int ms_NullOr(ms_VM *vm);

/*
 * MSCRIPT PRIMITIVE PROTOTYPES
 */

const ms_FuncDef MS_OBJECT_PROTOTYPE[] = {
    { NULL, NULL },
};

const ms_FuncDef MS_FLOAT_PROTOTYPE[] = {
    { "__str__", ms_FloatToStr },
    { "__float__", ms_FloatToFloat },
    { "__int__", ms_FloatToInt },
    { "__bool__", ms_FloatToBool },
    { "__add__", ms_FloatAdd },
    { "__sub__", ms_FloatSubtract },
    { "__mult__", ms_FloatMultiply },
    { "__div__", ms_FloatDivide },
    { "__idiv__", ms_FloatIDivide },
    { "__mod__", ms_FloatModulo },
    { "__exp__", ms_FloatExponentiate },
    { "__neg__", ms_FloatNegate },
    { "__lt__", ms_FloatLessThan },
    { "__le__", ms_FloatLessEqual },
    { "__gt__", ms_FloatGreaterThan },
    { "__ge__", ms_FloatGreaterEqual },
    { "__eq__", ms_FloatEqual },
    { "__ne__", ms_FloatNotEqual },
    { "__not__", ms_FloatNot },
    { "__and__", ms_FloatAnd },
    { "__or__", ms_FloatOr },
    { NULL, NULL },
};

const ms_FuncDef MS_INT_PROTOTYPE[] = {
    { "__str__", ms_IntToStr },
    { "__float__", ms_IntToFloat },
    { "__int__", ms_IntToInt },
    { "__bool__", ms_IntToBool },
    { "__add__", ms_IntAdd },
    { "__sub__", ms_IntSubtract },
    { "__mult__", ms_IntMultiply },
    { "__div__", ms_IntDivide },
    { "__idiv__", ms_IntIDivide },
    { "__mod__", ms_IntModulo },
    { "__exp__", ms_IntExponentiate },
    { "__neg__", ms_IntNegate },
    { "__lshift__", ms_IntLShift },
    { "__rshift__", ms_IntRShift },
    { "__band__", ms_IntBitwiseAnd },
    { "__bxor__", ms_IntBitwiseXor },
    { "__bor__", ms_IntBitwiseOr },
    { "__bnot__", ms_IntBitwiseNot },
    { "__lt__", ms_IntLessThan },
    { "__le__", ms_IntLessEqual },
    { "__gt__", ms_IntGreaterThan },
    { "__ge__", ms_IntGreaterEqual },
    { "__eq__", ms_IntEqual },
    { "__ne__", ms_IntNotEqual },
    { "__not__", ms_IntNot },
    { "__and__", ms_IntAnd },
    { "__or__", ms_IntOr },
    { NULL, NULL },
};

const ms_FuncDef MS_STR_PROTOTYPE[] = {
    { "__str__", ms_StrToStr },
    { "__float__", ms_StrToFloat },
    { "__int__", ms_StrToInt },
    { "__bool__", ms_StrToBool },
    { "__add__", ms_StrAdd },
    { "__lt__", ms_StrLessThan },
    { "__le__", ms_StrLessEqual },
    { "__gt__", ms_StrGreaterThan },
    { "__ge__", ms_StrGreaterEqual },
    { "__eq__", ms_StrEqual },
    { "__ne__", ms_StrNotEqual },
    { "__not__", ms_StrNot },
    { "__and__", ms_StrAnd },
    { "__or__", ms_StrOr },
    { NULL, NULL },
};

const ms_FuncDef MS_BOOL_PROTOTYPE[] = {
    { "__str__", ms_BoolToStr },
    { "__float__", ms_BoolToFloat },
    { "__int__", ms_BoolToInt },
    { "__bool__", ms_BoolToBool },
    { "__add__", ms_BoolAdd },
    { "__sub__", ms_BoolSubtract },
    { "__mult__", ms_BoolMultiply },
    { "__div__", ms_BoolDivide },
    { "__idiv__", ms_BoolIDivide },
    { "__mod__", ms_BoolModulo },
    { "__exp__", ms_BoolExponentiate },
    { "__neg__", ms_BoolNegate },
    { "__lshift__", ms_BoolLShift },
    { "__rshift__", ms_BoolRShift },
    { "__band__", ms_BoolBitwiseAnd },
    { "__bxor__", ms_BoolBitwiseXor },
    { "__bor__", ms_BoolBitwiseOr },
    { "__bnot__", ms_BoolBitwiseNot },
    { "__lt__", ms_BoolLessThan },
    { "__le__", ms_BoolLessEqual },
    { "__gt__", ms_BoolGreaterThan },
    { "__ge__", ms_BoolGreaterEqual },
    { "__eq__", ms_BoolEqual },
    { "__ne__", ms_BoolNotEqual },
    { "__not__", ms_BoolNot },
    { "__and__", ms_BoolAnd },
    { "__or__", ms_BoolOr },
    { NULL, NULL },
};

const ms_FuncDef MS_NULL_PROTOTYPE[] = {
    { "__str__", ms_NullToStr },
    { "__float__", ms_NullToFloat },
    { "__int__", ms_NullToInt },
    { "__bool__", ms_NullToBool },
    { "__eq__", ms_NullEqual },
    { "__ne__", ms_NullNotEqual },
    { "__not__", ms_NullNot },
    { "__and__", ms_NullAnd },
    { "__or__", ms_NullOr },
    { NULL, NULL },
};

/*
 * FLOAT PROTOTYPE FUNCTIONS
 */

static int ms_FloatToStr(ms_VM *vm) {
    return 0;
}

static int ms_FloatToFloat(ms_VM *vm) {
    return 1;
}

static int ms_FloatToInt(ms_VM *vm) {
    assert(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_FLOAT);
    ms_VMPushInt(vm, (ms_ValInt)l.val.f);
    return 1;
}

static int ms_FloatToBool(ms_VM *vm) {
    assert(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_FLOAT);
    ms_VMPushBool(vm, (ms_ValBool)(l.val.f != 0.0));
    return 1;
}

static int ms_FloatAdd(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_FLOAT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushFloat(vm, l.val.f + (ms_ValFloat)r.val.i);
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushFloat(vm, l.val.f + r.val.f);
            return 1;
        case VMVAL_BOOL:
            ms_VMPushFloat(vm, l.val.f + (ms_ValFloat)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_FloatSubtract(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_FLOAT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushFloat(vm, l.val.f - (ms_ValFloat)r.val.i);
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushFloat(vm, l.val.f - r.val.f);
            return 1;
        case VMVAL_BOOL:
            ms_VMPushFloat(vm, l.val.f - (ms_ValFloat)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_FloatMultiply(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_FLOAT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushFloat(vm, l.val.f * (ms_ValFloat)r.val.i);
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushFloat(vm, l.val.f * r.val.f);
            return 1;
        case VMVAL_BOOL:
            ms_VMPushFloat(vm, l.val.f * (ms_ValFloat)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_FloatDivide(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_FLOAT);
    switch (r.type) {
        case VMVAL_INT:
            if (r.val.i == 0) { return 0; }
            ms_VMPushFloat(vm, l.val.f / (ms_ValFloat)r.val.i);
            return 1;
        case VMVAL_FLOAT:
            if (r.val.f == 0.0) { return 0; }
            ms_VMPushFloat(vm, l.val.f / r.val.f);
            return 1;
        case VMVAL_BOOL:
            if (r.val.b == false) { return 0; }
            ms_VMPushFloat(vm, l.val.f / (ms_ValFloat)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_FloatIDivide(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_FLOAT);
    switch (r.type) {
        case VMVAL_INT:
            if (r.val.i == 0) { return 0; }
            ms_VMPushInt(vm, (ms_ValInt)l.val.f / r.val.i);
            return 1;
        case VMVAL_FLOAT:
            if (r.val.f == 0.0) { return 0; }
            ms_VMPushInt(vm, (ms_ValInt)trunc(l.val.f) / (ms_ValInt)trunc(r.val.f));
            return 1;
        case VMVAL_BOOL:
            if (r.val.b == false) { return 0; }
            ms_VMPushInt(vm, (ms_ValInt)l.val.f / (ms_ValInt)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_FloatModulo(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_FLOAT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushFloat(vm, (ms_ValFloat)fmod(l.val.f, (ms_ValFloat)r.val.i));
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushFloat(vm, (ms_ValFloat)fmod(l.val.f, r.val.f));
            return 1;
        case VMVAL_BOOL:
            ms_VMPushFloat(vm, (ms_ValFloat)fmod(l.val.f, (ms_ValFloat)r.val.b));
            return 1;
        default:
            return 0;
    }
}

static int ms_FloatExponentiate(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_FLOAT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushFloat(vm, (ms_ValFloat)pow(l.val.f, (ms_ValFloat)r.val.i));
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushFloat(vm, (ms_ValFloat)pow(l.val.f, r.val.f));
            return 1;
        case VMVAL_BOOL:
            ms_VMPushFloat(vm, (ms_ValFloat)pow(l.val.f, (ms_ValFloat)r.val.b));
            return 1;
        default:
            return 0;
    }
}

static int ms_FloatNegate(ms_VM *vm) {
    assert(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_FLOAT);
    ms_VMPushFloat(vm, -l.val.f);
    return 1;
}

static int ms_FloatLessThan(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_FLOAT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.f < (ms_ValFloat)r.val.i));
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.f < r.val.f));
            return 1;
        case VMVAL_BOOL:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.f < (ms_ValFloat)r.val.b));
            return 1;
        default:
            return 0;
    }
}

static int ms_FloatLessEqual(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_FLOAT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.f <= (ms_ValFloat)r.val.i));
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.f <= r.val.f));
            return 1;
        case VMVAL_BOOL:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.f <= (ms_ValFloat)r.val.b));
            return 1;
        default:
            return 0;
    }
}

static int ms_FloatGreaterThan(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_FLOAT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.f > (ms_ValFloat)r.val.i));
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.f > r.val.f));
            return 1;
        case VMVAL_BOOL:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.f > (ms_ValFloat)r.val.b));
            return 1;
        default:
            return 0;
    }
}

static int ms_FloatGreaterEqual(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_FLOAT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.f >= (ms_ValFloat)r.val.i));
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.f >= r.val.f));
            return 1;
        case VMVAL_BOOL:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.f >= (ms_ValFloat)r.val.b));
            return 1;
        default:
            return 0;
    }
}

static int ms_FloatEqual(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_FLOAT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.f == (ms_ValFloat)r.val.i));
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.f == r.val.f));
            return 1;
        case VMVAL_BOOL:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.f == (ms_ValFloat)r.val.b));
            return 1;
        default:
            return 0;
    }
}

static int ms_FloatNotEqual(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_FLOAT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.f != (ms_ValFloat)r.val.i));
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.f != r.val.f));
            return 1;
        case VMVAL_BOOL:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.f != (ms_ValFloat)r.val.b));
            return 1;
        default:
            return 0;
    }
}

static int ms_FloatNot(ms_VM *vm) {
    assert(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_FLOAT);
    ms_VMPushBool(vm, !(ms_ValBool)l.val.f);
    return 1;
}

static int ms_FloatAnd(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_FLOAT);

    ms_VMPush(vm, r);
    ms_Function tobool = ms_VMPrototypeFuncGet(vm, r.type, "__bool__");
    if ((tobool) && (tobool(vm) == 1)) {
        r = ms_VMPop(vm);
        ms_VMPushBool(vm, (ms_ValBool)l.val.f && r.val.b);
        return 1;
    }

    (void)ms_VMPop(vm);
    return 0;
}

static int ms_FloatOr(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_FLOAT);
    if ((ms_ValBool)l.val.f) {
        ms_VMPushBool(vm, true);
        return 1;
    }

    ms_VMPush(vm, r);
    ms_Function tobool = ms_VMPrototypeFuncGet(vm, r.type, "__bool__");
    if ((tobool) && (tobool(vm) == 1)) {
        r = ms_VMPop(vm);
        ms_VMPushBool(vm, r.val.b);
        return 1;
    }

    (void)ms_VMPop(vm);
    return 0;
}

/*
 * INT PROTOTYPE FUNCTIONS
 */

static int ms_IntToStr(ms_VM *vm) {
    return 0;
}

static int ms_IntToFloat(ms_VM *vm) {
    assert(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);
    ms_VMPushFloat(vm, (ms_ValFloat)l.val.i);
    return 1;
}

static int ms_IntToInt(ms_VM *vm) {
    return 1;
}

static int ms_IntToBool(ms_VM *vm) {
    assert(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);
    ms_VMPushBool(vm, (ms_ValBool)(l.val.i != 0));
    return 1;
}

static int ms_IntAdd(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushInt(vm, l.val.i + r.val.i);
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushFloat(vm, (ms_ValFloat)l.val.i + r.val.f);
            return 1;
        case VMVAL_BOOL:
            ms_VMPushInt(vm, l.val.i + (ms_ValInt)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_IntSubtract(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushInt(vm, l.val.i - r.val.i);
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushFloat(vm, (ms_ValFloat)l.val.i - r.val.f);
            return 1;
        case VMVAL_BOOL:
            ms_VMPushInt(vm, l.val.i - (ms_ValInt)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_IntMultiply(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushInt(vm, l.val.i * r.val.i);
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushFloat(vm, (ms_ValFloat)l.val.i * r.val.f);
            return 1;
        case VMVAL_BOOL:
            ms_VMPushInt(vm, l.val.i * (ms_ValInt)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_IntDivide(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);
    switch (r.type) {
        case VMVAL_INT:
            if (r.val.i == 0) { return 0; }
            ms_VMPushInt(vm, l.val.i / r.val.i);
            return 1;
        case VMVAL_FLOAT:
            if (r.val.f == 0.0) { return 0; }
            ms_VMPushFloat(vm, (ms_ValFloat)l.val.i / r.val.f);
            return 1;
        case VMVAL_BOOL:
            if (r.val.b == false) { return 0; }
            ms_VMPushInt(vm, l.val.i / (ms_ValInt)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_IntIDivide(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);
    switch (r.type) {
        case VMVAL_INT:
            if (r.val.i == 0) { return 0; }
            ms_VMPushInt(vm, l.val.i / r.val.i);
            return 1;
        case VMVAL_FLOAT:
            if (r.val.f == 0.0) { return 0; }
            ms_VMPushFloat(vm, l.val.i / (ms_ValInt)r.val.f);
            return 1;
        case VMVAL_BOOL:
            if (r.val.b == false) { return 0; }
            ms_VMPushInt(vm, l.val.i / (ms_ValInt)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_IntModulo(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);
    switch (r.type) {
        case VMVAL_INT:
            if (l.val.i == 0) { return 0; }
            ms_VMPushInt(vm, l.val.i % r.val.i);
            return 1;
        case VMVAL_FLOAT:
            if (l.val.f == 0.0) { return 0; }
            ms_VMPushFloat(vm, (ms_ValFloat)fmod((ms_ValFloat)l.val.i, r.val.f));
            return 1;
        case VMVAL_BOOL:
            if (l.val.b == false) { return 0; }
            ms_VMPushInt(vm, l.val.i % (ms_ValInt)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_IntExponentiate(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushInt(vm, (ms_ValInt)pow((ms_ValFloat)l.val.i, (ms_ValFloat)r.val.i));
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushFloat(vm, (ms_ValFloat)pow((ms_ValFloat)l.val.i, r.val.f));
            return 1;
        case VMVAL_BOOL:
            ms_VMPushInt(vm, (ms_ValInt)pow((ms_ValFloat)l.val.i, (ms_ValFloat)r.val.b));
            return 1;
        default:
            return 0;
    }
}

static int ms_IntNegate(ms_VM *vm) {
    assert(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);
    ms_VMPushInt(vm, -l.val.i);
    return 1;
}

static int ms_IntLShift(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushInt(vm, l.val.i << r.val.i);
            return 1;
        case VMVAL_BOOL:
            ms_VMPushInt(vm, l.val.i << (ms_ValInt)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_IntRShift(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushInt(vm, l.val.i >> r.val.i);
            return 1;
        case VMVAL_BOOL:
            ms_VMPushInt(vm, l.val.i >> (ms_ValInt)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_IntBitwiseAnd(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushInt(vm, l.val.i & r.val.i);
            return 1;
        case VMVAL_BOOL:
            ms_VMPushInt(vm, l.val.i & (ms_ValInt)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_IntBitwiseXor(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushInt(vm, l.val.i ^ r.val.i);
            return 1;
        case VMVAL_BOOL:
            ms_VMPushInt(vm, l.val.i ^ (ms_ValInt)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_IntBitwiseOr(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushInt(vm, l.val.i | r.val.i);
            return 1;
        case VMVAL_BOOL:
            ms_VMPushInt(vm, l.val.i | (ms_ValInt)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_IntBitwiseNot(ms_VM *vm) {
    assert(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);
    ms_VMPushInt(vm, ~l.val.i);
    return 1;
}

static int ms_IntLessThan(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.i < r.val.i));
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushBool(vm, (ms_ValBool)((ms_ValFloat)l.val.i < r.val.f));
            return 1;
        case VMVAL_BOOL:
            ms_VMPushInt(vm, (ms_ValBool)(l.val.i < (ms_ValInt)r.val.b));
            return 1;
        default:
            return 0;
    }
}

static int ms_IntLessEqual(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.i <= r.val.i));
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushBool(vm, (ms_ValBool)((ms_ValFloat)l.val.i <= r.val.f));
            return 1;
        case VMVAL_BOOL:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.i <= (ms_ValInt)r.val.b));
            return 1;
        default:
            return 0;
    }
}

static int ms_IntGreaterThan(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.i > r.val.i));
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushBool(vm, (ms_ValBool)((ms_ValFloat)l.val.i > r.val.f));
            return 1;
        case VMVAL_BOOL:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.i > (ms_ValInt)r.val.b));
            return 1;
        default:
            return 0;
    }
}

static int ms_IntGreaterEqual(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.i >= r.val.i));
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushBool(vm, (ms_ValBool)((ms_ValFloat)l.val.i >= r.val.f));
            return 1;
        case VMVAL_BOOL:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.i >= (ms_ValInt)r.val.b));
            return 1;
        default:
            return 0;
    }
}

static int ms_IntEqual(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.i == r.val.i));
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushBool(vm, (ms_ValBool)((ms_ValFloat)l.val.i == r.val.f));
            return 1;
        case VMVAL_BOOL:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.i == (ms_ValInt)r.val.b));
            return 1;
        default:
            return 0;
    }
}

static int ms_IntNotEqual(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.i != r.val.i));
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushBool(vm, (ms_ValBool)((ms_ValFloat)l.val.i != r.val.f));
            return 1;
        case VMVAL_BOOL:
            ms_VMPushBool(vm, (ms_ValBool)(l.val.i != (ms_ValInt)r.val.b));
            return 1;
        default:
            return 0;
    }
}

static int ms_IntNot(ms_VM *vm) {
    assert(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);
    ms_VMPushBool(vm, !(ms_ValBool)l.val.i);
    return 1;
}

static int ms_IntAnd(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);

    ms_VMPush(vm, r);
    ms_Function tobool = ms_VMPrototypeFuncGet(vm, r.type, "__bool__");
    if ((tobool) && (tobool(vm) == 1)) {
        r = ms_VMPop(vm);
        ms_VMPushBool(vm, (ms_ValBool)l.val.i && r.val.b);
        return 1;
    }

    (void)ms_VMPop(vm);
    return 0;
}

static int ms_IntOr(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_INT);
    if ((ms_ValBool)l.val.i) {
        ms_VMPushBool(vm, true);
        return 1;
    }

    ms_VMPush(vm, r);
    ms_Function tobool = ms_VMPrototypeFuncGet(vm, r.type, "__bool__");
    if ((tobool) && (tobool(vm) == 1)) {
        r = ms_VMPop(vm);
        ms_VMPushBool(vm, r.val.b);
        return 1;
    }

    (void)ms_VMPop(vm);
    return 0;
}

/*
 * STR PROTOTYPE FUNCTIONS
 */

static int ms_StrToStr(ms_VM *vm) {
    return 1;
}

static int ms_StrToFloat(ms_VM *vm) {
    return 0;
}

static int ms_StrToInt(ms_VM *vm) {
    return 0;
}

static int ms_StrToBool(ms_VM *vm) {
    return 0;
}

static int ms_StrAdd(ms_VM *vm) {
    return 0;
}

static int ms_StrLessThan(ms_VM *vm) {
    return 0;
}

static int ms_StrLessEqual(ms_VM *vm) {
    return 0;
}

static int ms_StrGreaterThan(ms_VM *vm) {
    return 0;
}

static int ms_StrGreaterEqual(ms_VM *vm) {
    return 0;
}

static int ms_StrEqual(ms_VM *vm) {
    return 0;
}

static int ms_StrNotEqual(ms_VM *vm) {
    return 0;
}

static int ms_StrNot(ms_VM *vm) {
    return 0;
}

static int ms_StrAnd(ms_VM *vm) {
    return 0;
}

static int ms_StrOr(ms_VM *vm) {
    return 0;
}

/*
 * BOOL PROTOTYPE FUNCTIONS
 */

static int ms_BoolToStr(ms_VM *vm) {
    return 0;
}

static int ms_BoolToFloat(ms_VM *vm) {
    assert(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_BOOL);
    ms_VMPushFloat(vm, (ms_ValFloat)l.val.b);
    return 1;
}

static int ms_BoolToInt(ms_VM *vm) {
    assert(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_BOOL);
    ms_VMPushInt(vm, (ms_ValInt)l.val.b);
    return 1;
}

static int ms_BoolToBool(ms_VM *vm) {
    return 1;
}

static int ms_BoolAdd(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_BOOL);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushInt(vm, (ms_ValInt)l.val.b + r.val.i);
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushFloat(vm, (ms_ValFloat)l.val.b + r.val.f);
            return 1;
        case VMVAL_BOOL:
            ms_VMPushInt(vm, (ms_ValInt)l.val.b + (ms_ValInt)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_BoolSubtract(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_BOOL);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushInt(vm, (ms_ValInt)l.val.b - r.val.i);
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushFloat(vm, (ms_ValFloat)l.val.b - r.val.f);
            return 1;
        case VMVAL_BOOL:
            ms_VMPushInt(vm, (ms_ValInt)l.val.b - (ms_ValInt)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_BoolMultiply(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_BOOL);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushInt(vm, (ms_ValInt)l.val.b * r.val.i);
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushFloat(vm, (ms_ValFloat)l.val.b * r.val.f);
            return 1;
        case VMVAL_BOOL:
            ms_VMPushInt(vm, (ms_ValInt)l.val.b * (ms_ValInt)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_BoolDivide(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_BOOL);
    switch (r.type) {
        case VMVAL_INT:
            if (r.val.i == 0) { return 0; }
            ms_VMPushInt(vm, (ms_ValInt)l.val.b / r.val.i);
            return 1;
        case VMVAL_FLOAT:
            if (r.val.f == 0.0) { return 0; }
            ms_VMPushFloat(vm, (ms_ValFloat)l.val.b / r.val.f);
            return 1;
        case VMVAL_BOOL:
            if (r.val.b == false) { return 0; }
            ms_VMPushInt(vm, (ms_ValInt)l.val.b / (ms_ValInt)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_BoolIDivide(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_BOOL);
    switch (r.type) {
        case VMVAL_INT:
            if (r.val.i == 0) { return 0; }
            ms_VMPushInt(vm, (ms_ValInt)l.val.b / r.val.i);
            return 1;
        case VMVAL_FLOAT:
            if (r.val.f == 0.0) { return 0; }
            ms_VMPushFloat(vm, (ms_ValInt)(l.val.b / (ms_ValInt)r.val.f));
            return 1;
        case VMVAL_BOOL:
            if (r.val.b == false) { return 0; }
            ms_VMPushInt(vm, (ms_ValInt)l.val.b / (ms_ValInt)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_BoolModulo(ms_VM *vm) {
    return 1;
}

static int ms_BoolExponentiate(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_BOOL);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushInt(vm, (ms_ValInt)pow((ms_ValFloat)l.val.b, (ms_ValFloat)r.val.i));
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushInt(vm, (ms_ValInt)pow((ms_ValFloat)l.val.b, r.val.f));
            return 1;
        case VMVAL_BOOL:
            ms_VMPushInt(vm, (ms_ValInt)pow((ms_ValFloat)l.val.b, (ms_ValFloat)r.val.b));
            return 1;
        default:
            return 0;
    }
}

static int ms_BoolNegate(ms_VM *vm) {
    assert(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_BOOL);
    ms_VMPushInt(vm, -((ms_ValInt)l.val.b));
    return 1;
}

static int ms_BoolLShift(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_BOOL);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushInt(vm, (ms_ValInt)l.val.b << r.val.i);
            return 1;
        case VMVAL_BOOL:
            ms_VMPushInt(vm, (ms_ValInt)l.val.b << (ms_ValInt)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_BoolRShift(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_BOOL);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushInt(vm, (ms_ValInt)l.val.b >> r.val.i);
            return 1;
        case VMVAL_BOOL:
            ms_VMPushInt(vm, (ms_ValInt)l.val.b >> (ms_ValInt)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_BoolBitwiseAnd(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_BOOL);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushInt(vm, (ms_ValInt)l.val.b & r.val.i);
            return 1;
        case VMVAL_BOOL:
            ms_VMPushInt(vm, (ms_ValInt)l.val.b & (ms_ValInt)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_BoolBitwiseXor(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_BOOL);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushInt(vm, (ms_ValInt)l.val.b ^ r.val.i);
            return 1;
        case VMVAL_BOOL:
            ms_VMPushInt(vm, (ms_ValInt)l.val.b ^ (ms_ValInt)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_BoolBitwiseOr(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_BOOL);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushInt(vm, (ms_ValInt)l.val.b | r.val.i);
            return 1;
        case VMVAL_BOOL:
            ms_VMPushInt(vm, (ms_ValInt)l.val.b | (ms_ValInt)r.val.b);
            return 1;
        default:
            return 0;
    }
}

static int ms_BoolBitwiseNot(ms_VM *vm) {
    assert(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_BOOL);
    ms_VMPushInt(vm, ~((ms_ValInt)l.val.b));
    return 1;
}

static int ms_BoolLessThan(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_BOOL);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushBool(vm, (ms_ValBool)((ms_ValInt)l.val.b < r.val.i));
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushBool(vm, (ms_ValBool)((ms_ValFloat)l.val.b < r.val.f));
            return 1;
        case VMVAL_BOOL:
            ms_VMPushBool(vm, (ms_ValBool)((ms_ValInt)l.val.b < (ms_ValInt)r.val.b));
            return 1;
        default:
            return 0;
    }
}

static int ms_BoolLessEqual(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_BOOL);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushBool(vm, (ms_ValBool)((ms_ValInt)l.val.b <= r.val.i));
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushBool(vm, (ms_ValBool)((ms_ValFloat)l.val.b <= r.val.f));
            return 1;
        case VMVAL_BOOL:
            ms_VMPushBool(vm, (ms_ValBool)((ms_ValInt)l.val.b <= (ms_ValInt)r.val.b));
            return 1;
        default:
            return 0;
    }
}

static int ms_BoolGreaterThan(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_BOOL);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushBool(vm, (ms_ValBool)((ms_ValInt)l.val.b > r.val.i));
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushBool(vm, (ms_ValBool)((ms_ValFloat)l.val.b > r.val.f));
            return 1;
        case VMVAL_BOOL:
            ms_VMPushBool(vm, (ms_ValBool)((ms_ValInt)l.val.b > (ms_ValInt)r.val.b));
            return 1;
        default:
            return 0;
    }
}

static int ms_BoolGreaterEqual(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_BOOL);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushBool(vm, (ms_ValBool)((ms_ValInt)l.val.b >= r.val.i));
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushBool(vm, (ms_ValBool)((ms_ValFloat)l.val.b >= r.val.f));
            return 1;
        case VMVAL_BOOL:
            ms_VMPushBool(vm, (ms_ValBool)((ms_ValInt)l.val.b >= (ms_ValInt)r.val.b));
            return 1;
        default:
            return 0;
    }
}

static int ms_BoolEqual(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_BOOL);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushBool(vm, (ms_ValBool)((ms_ValInt)l.val.b == r.val.i));
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushBool(vm, (ms_ValBool)((ms_ValFloat)l.val.b == r.val.f));
            return 1;
        case VMVAL_BOOL:
            ms_VMPushBool(vm, (ms_ValBool)((ms_ValInt)l.val.b == (ms_ValInt)r.val.b));
            return 1;
        default:
            return 0;
    }
}

static int ms_BoolNotEqual(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_BOOL);
    switch (r.type) {
        case VMVAL_INT:
            ms_VMPushBool(vm, (ms_ValBool)((ms_ValInt)l.val.b != r.val.i));
            return 1;
        case VMVAL_FLOAT:
            ms_VMPushBool(vm, (ms_ValBool)((ms_ValFloat)l.val.b != r.val.f));
            return 1;
        case VMVAL_BOOL:
            ms_VMPushInt(vm, (ms_ValBool)((ms_ValInt)l.val.b != (ms_ValInt)r.val.b));
            return 1;
        default:
            return 0;
    }
}

static int ms_BoolNot(ms_VM *vm) {
    assert(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_BOOL);
    ms_VMPushBool(vm, !((ms_ValBool)l.val.b));
    return 1;
}

static int ms_BoolAnd(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_BOOL);

    ms_VMPush(vm, r);
    ms_Function tobool = ms_VMPrototypeFuncGet(vm, r.type, "__bool__");
    if ((tobool) && (tobool(vm) == 1)) {
        r = ms_VMPop(vm);
        ms_VMPushBool(vm, l.val.b && r.val.b);
        return 1;
    }

    (void)ms_VMPop(vm);
    return 0;
}

static int ms_BoolOr(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_BOOL);
    if (l.val.b) {
        ms_VMPushBool(vm, true);
        return 1;
    }

    ms_VMPush(vm, r);
    ms_Function tobool = ms_VMPrototypeFuncGet(vm, r.type, "__bool__");
    if ((tobool) && (tobool(vm) == 1)) {
        r = ms_VMPop(vm);
        ms_VMPushBool(vm, r.val.b);
        return 1;
    }

    (void)ms_VMPop(vm);
    return 0;
}

/*
 * NULL PROTOTYPE FUNCTIONS
 */

static int ms_NullToStr(ms_VM *vm) {
    assert(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_NULL);
    ms_VMPushStrL(vm, "null", 4);
    return 1;
}

static int ms_NullToFloat(ms_VM *vm) {
    assert(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_NULL);
    ms_VMPushFloat(vm, 0.0);
    return 1;
}

static int ms_NullToInt(ms_VM *vm) {
    assert(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_NULL);
    ms_VMPushInt(vm, 0);
    return 1;
}

static int ms_NullToBool(ms_VM *vm) {
    assert(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_NULL);
    ms_VMPushBool(vm, false);
    return 1;
}

static int ms_NullEqual(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_NULL);
    ms_VMPushBool(vm, (ms_ValBool)(r.type == VMVAL_NULL));
    return 1;
}

static int ms_NullNotEqual(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_NULL);
    ms_VMPushBool(vm, (ms_ValBool)(r.type != VMVAL_NULL));
    return 1;
}

static int ms_NullNot(ms_VM *vm) {
    assert(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_NULL);
    ms_VMPushBool(vm, true);
    return 1;
}

static int ms_NullAnd(ms_VM *vm) {
    assert(vm);
    (void)ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_NULL);
    ms_VMPushBool(vm, false);
    return 1;
}

static int ms_NullOr(ms_VM *vm) {
    assert(vm);
    ms_VMValue r = ms_VMPop(vm);
    ms_VMValue l = ms_VMPop(vm);
    assert(l.type == VMVAL_NULL);

    ms_VMPush(vm, r);
    ms_Function tobool = ms_VMPrototypeFuncGet(vm, r.type, "__bool__");
    if ((tobool) && (tobool(vm) == 1)) {
        r = ms_VMPop(vm);
        ms_VMPushBool(vm, r.val.b);
        return 1;
    }

    (void)ms_VMPop(vm);
    return 0;
}
