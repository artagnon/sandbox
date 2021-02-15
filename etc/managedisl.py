from __future__ import print_function

__copyright__ = """
Copyright (C) 2016 The MathWorks, Inc.
Copyright (C) 2011-15 Andreas Kloeckner
"""

__license__ = """
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
"""

import re
import sys
import copy
from py_codegen import PythonCodeGenerator, Indentation

SEM_TAKE = "take"
SEM_GIVE = "give"
SEM_KEEP = "keep"
SEM_NULL = "null"

ISL_SEM_TO_SEM = {
    "__isl_take": SEM_TAKE,
    "__isl_give": SEM_GIVE,
    "__isl_keep": SEM_KEEP,
    "__isl_null": SEM_NULL,
    }

NON_COPYABLE = ["ctx", "printer", "access_info", "vertex", "vertices", "cell",
                "flow", "restriction", "union_flow"]
NON_COPYABLE_WITH_ISL_PREFIX = ["isl_"+i for i in NON_COPYABLE]

CPP_RESERVED_WORDS = """
and	        and_eq	          asm	        auto	     bitand
bitor	    bool	          break	        case	     catch
char	    class	          const	        const_cast	 continue
default	    delete	          do	        double	     dynamic_cast
else	    enum	          explicit	    export	     extern
false	    float	          for	        friend	     goto
if	        inline	          int	        long	     mutable
namespace	new               not	        not_eq	     operator
or	        or_eq	          private	    protected    public
register	reinterpret_cast  return	    short	     signed
sizeof	    static	          static_cast	struct	     switch
template	this	          throw	        true	     try
typedef	    typeid	          typename	    union	     unsigned
using   	virtual	          void	        volatile	 wchar_t
while       xor               xor_eq
""".split()


# {{{ data model

def type_cdecl(base_type, type_ptr, decl_words):
    return "{decl_words}{optional_space}{base_type}{optional_space2}{ptr}".format(
        decl_words=" ".join(decl_words),
        optional_space=" " if len(decl_words) > 0 else "",
        base_type=base_type,
        optional_space2=" " if len(type_ptr) > 0 else "",
        ptr=type_ptr)

def type_cpp_cls(base_type, type_ptr, decl_words):
    # whether it's isl_set* or isl_set**, we return the same `Set` since we're passing by reference
    if base_type.startswith("isl_") and (type_ptr == "*" or type_ptr == "**"):
        return isl_class_to_cpp_class(base_type)
    # there are no char** things in isl, except some obscure argv thing we won't consider
    elif base_type in ["const char", "char"] and type_ptr == "*":
        return "std::string"
    # fallback c_declarator option
    return type_cdecl(base_type, type_ptr, decl_words)

class Argument:
    def __init__(self, name, semantics, decl_words, base_type, ptr):
        self.name = name
        self.semantics = semantics
        assert isinstance(decl_words, list)
        self.decl_words = decl_words
        self.base_type = base_type
        self.ptr = ptr

    def c_declarator(self):
        return "{cdecl}{name}".format(
                cdecl=type_cdecl(self.base_type, self.ptr, self.decl_words),
                name=self.name)

    def cpp_cls(self):
        ty = type_cpp_cls(self.base_type, self.ptr, self.decl_words)
        # whether it's isl_set* or isl_set**, it makes no difference to us
        if self.base_type.startswith("isl_"):
            if self.base_type[4:] in NON_COPYABLE:
                return "{type} &{name}".format(type=ty, name=self.name)
            elif self.semantics == SEM_TAKE:
                return "{type} &&{name}".format(type=ty, name=self.name)
        return "{type} {name}".format(type=ty, name=self.name)

class CallbackArgument:
    def __init__(self, name,
            return_semantics, return_decl_words, return_base_type, return_ptr, args):
        self.name = name
        self.return_semantics = return_semantics
        self.semantics = SEM_KEEP
        assert isinstance(return_decl_words, list)
        self.return_decl_words = return_decl_words
        self.return_base_type = return_base_type
        self.base_type = return_base_type
        self.return_ptr = return_ptr
        self.ptr = return_ptr
        self.args = args

    def c_declarator(self):
        return "{cdecl} (*{name})({args})".format(
                cdecl=type_cdecl(self.return_base_type, self.return_ptr, self.return_decl_words),
                name=self.name,
                args=", ".join([arg.c_declarator() for arg in self.args]))

    def c_declarator_type(self):
        return "{cdecl} (*)({args})".format(
                cdecl=type_cdecl(self.return_base_type, self.return_ptr, self.return_decl_words),
                name=self.name,
                args=", ".join([arg.c_declarator() for arg in self.args]))

    def cpp_cls(self):
        return "{type_cpp_cls} (*{name})({args_cpp_cls})".format(
            type_cpp_cls=type_cpp_cls(self.return_base_type, self.return_ptr, self.return_decl_words),
            name=self.name,
            args_cpp_cls=self.args_cpp_cls())

    def args_cpp_cls(self):
        return ", ".join([arg.cpp_cls() for arg in self.args])

    def ret_cpp_cls(self):
        return type_cpp_cls(self.return_base_type, self.return_ptr, self.return_decl_words)

class Method:
    def __init__(self, cls, name, c_name,
            return_semantics, return_decl_words, return_base_type, return_ptr,
            args, is_exported, is_constructor):
        self.cls = cls
        self.name = name
        self.c_name = c_name
        self.return_semantics = return_semantics
        self.return_decl_words = return_decl_words
        self.return_base_type = return_base_type
        self.return_ptr = return_ptr
        self.args = args
        self.mutator_veto = False
        self.is_exported = is_exported
        self.is_constructor = is_constructor

        if not self.is_static:
            self.args[0].name = "*this"

    @property
    def is_static(self):
        return not (self.args and self.args[0].base_type.startswith("isl_"+self.cls))

    @property
    def is_mutator(self):
        return (not self.is_static
                and self.args[0].semantics is SEM_TAKE
                and self.return_ptr == "*" == self.args[0].ptr
                and self.return_base_type == self.args[0].base_type
                and self.return_semantics is SEM_GIVE
                and not self.mutator_veto
                and self.args[0].base_type in NON_COPYABLE_WITH_ISL_PREFIX)

    def ret_cpp_cls(self):
        ty = type_cpp_cls(self.return_base_type, self.return_ptr, self.return_decl_words)
        return ty

    def args_cpp_cls(self):
        return ", ".join([arg.cpp_cls() for arg in self.args])

    def prune_this(self):
        meth = copy.copy(self)
        if len(meth.args) > 0 and meth.args[0].base_type == "isl_"+meth.cls:
            # The first argument is the implicit "this", which is useful while calling the isl version
            # of the function, but not useful when declaring or defining the cpp version.
            meth.args = meth.args[1:]
            return meth
        return self

    def cpp_cls(self):
        static_spec = "static " if self.is_static else ""
        return "{static_spec}{ret_cpp_cls} {name}({args});".format(
            static_spec=static_spec,
            ret_cpp_cls=self.ret_cpp_cls(),
            name=self.name, # cpp name, not c_name
            args=self.prune_this().args_cpp_cls())

    def __repr__(self):
        return "<method %s>" % self.c_name

# }}}


CLASSES = [
        # /!\ Order matters, class names that are prefixes of others should go last.

        "ctx",

        # lists
        "id_list", "val_list",
        "basic_set_list", "basic_map_list", "set_list", "map_list",
        "union_set_list",
        "constraint_list",
        "aff_list", "pw_aff_list", "band_list",
        "ast_expr_list", "ast_node_list",

        # maps
        "id_to_ast_expr",

        # others
        "printer",  "val", "multi_val", "vec", "mat",
        "aff", "pw_aff", "union_pw_aff",
        "multi_aff", "multi_pw_aff", "pw_multi_aff", "union_pw_multi_aff",
        "union_pw_aff_list",
        "multi_union_pw_aff",

        "id",
        "constraint", "space", "local_space",

        "basic_set", "basic_map",
        "set", "map",
        "union_map", "union_set",
        "point", "vertex", "cell", "vertices",

        "qpolynomial_fold", "pw_qpolynomial_fold",
        "union_pw_qpolynomial_fold",
        "union_pw_qpolynomial",
        "qpolynomial", "pw_qpolynomial",
        "term",

        "band", "schedule_constraints", "schedule_node", "schedule",

        "access_info", "flow", "restriction",
        "union_access_info", "union_flow",

        "ast_expr", "ast_node", "ast_print_options",
        "ast_build",
        ]

UNTYPEDEFD_CLASSES = ["options"]


IMPLICIT_CONVERSIONS = {
    "isl_set": [("isl_basic_set", "from_basic_set")],
    "isl_map": [("isl_basic_map", "from_basic_map")],
    "isl_union_set": [("isl_set", "from_set")],
    "isl_union_map": [("isl_map", "from_map")],
    "isl_local_space": [("isl_space", "from_space")],
    "isl_pw_aff": [("isl_aff", "from_aff")],
    }


ENUMS = {
    # ctx.h
    "isl_error": """
        isl_error_none,
        isl_error_abort,
        isl_error_alloc,
        isl_error_unknown,
        isl_error_internal,
        isl_error_invalid,
        isl_error_quota,
        isl_error_unsupported,
    """,
    "isl_stat": """
        isl_stat_error,
        isl_stat_ok,
    """,
    "isl_bool": """
        isl_bool_error,
        isl_bool_false,
        isl_bool_true,
    """,
    # space.h
    "isl_dim_type": """
        isl_dim_cst,
        isl_dim_param,
        isl_dim_in,
        isl_dim_out,
        isl_dim_set,
        isl_dim_div,
        isl_dim_all,
    """,

    # schedule_type.h
    "isl_schedule_node_type": """
        isl_schedule_node_error,
        isl_schedule_node_band,
        isl_schedule_node_context,
        isl_schedule_node_domain,
        isl_schedule_node_expansion,
        isl_schedule_node_extension,
        isl_schedule_node_filter,
        isl_schedule_node_leaf,
        isl_schedule_node_guard,
        isl_schedule_node_mark,
        isl_schedule_node_sequence,
        isl_schedule_node_set,
    """,

    # ast_type.h
    "isl_ast_op_type": """
        isl_ast_op_error,
        isl_ast_op_and,
        isl_ast_op_and_then,
        isl_ast_op_or,
        isl_ast_op_or_else,
        isl_ast_op_max,
        isl_ast_op_min,
        isl_ast_op_minus,
        isl_ast_op_add,
        isl_ast_op_sub,
        isl_ast_op_mul,
        isl_ast_op_div,
        isl_ast_op_fdiv_q,
        isl_ast_op_pdiv_q,
        isl_ast_op_pdiv_r,
        isl_ast_op_zdiv_r,
        isl_ast_op_cond,
        isl_ast_op_select,
        isl_ast_op_eq,
        isl_ast_op_le,
        isl_ast_op_lt,
        isl_ast_op_ge,
        isl_ast_op_gt,
        isl_ast_op_call,
        isl_ast_op_access,
        isl_ast_op_member,
        isl_ast_op_address_of,
    """,
    "isl_ast_expr_type": """
        isl_ast_expr_error,
        isl_ast_expr_op,
        isl_ast_expr_id,
        isl_ast_expr_int,
    """,
    "isl_ast_node_type": """
        isl_ast_node_error,
        isl_ast_node_for,
        isl_ast_node_if,
        isl_ast_node_block,
        isl_ast_node_mark,
        isl_ast_node_user,
    """,
    "isl_ast_loop_type": """
        isl_ast_loop_error,
        isl_ast_loop_default,
        isl_ast_loop_atomic,
        isl_ast_loop_unroll,
        isl_ast_loop_separate,
    """,

    # polynomial_type.h
    "isl_fold": """
        isl_fold_min,
        isl_fold_max,
        isl_fold_list,
    """,

    # printer.h
    "isl_format": """
        ISL_FORMAT_ISL,
        ISL_FORMAT_POLYLIB,
        ISL_FORMAT_POLYLIB_CONSTRAINTS,
        ISL_FORMAT_OMEGA,
        ISL_FORMAT_C,
        ISL_FORMAT_LATEX,
        ISL_FORMAT_EXT_POLYLIB,
    """,

    "isl_yaml_style": """
        ISL_YAML_STYLE_BLOCK,
        ISL_YAML_STYLE_FLOW,
    """,

    # options.h

    "isl_bound": """
        ISL_BOUND_BERNSTEIN,
        ISL_BOUND_RANGE,
    """,

    "isl_on_error": """
        ISL_ON_ERROR_WARN,
        ISL_ON_ERROR_CONTINUE,
        ISL_ON_ERROR_ABORT,
    """,

    "isl_schedule_algorithm": """
        ISL_SCHEDULE_ALGORITHM_ISL,
        ISL_SCHEDULE_ALGORITHM_FEAUTRIER,
    """
    }

# isl_basic_set_multiplicative_call is actually blacklisted due to
# absence of "user" pointer in callback
DEPRECATED_METHODS = [ "isl_space_tuple_match",
                       "isl_basic_set_add",
                       "isl_basic_set_multiplicative_call" ]

MACRO_ENUMS = [
        "isl_format", "isl_yaml_style",
        "isl_bound", "isl_on_error", "isl_schedule_algorithm",
        ]

SPECIAL_CLASS_NAME_MAP = {
        "ctx": "Context"
        }

def isl_class_to_cpp_class(cls_name):
    if cls_name.startswith("isl_"):
        cls_name = cls_name[4:]
    try:
        return SPECIAL_CLASS_NAME_MAP[cls_name]
    except KeyError:
        return ''.join(piece.capitalize() for piece in cls_name.split('_'))

HEADER_PREAMBLE = """
#pragma once

#include "isl/ctx.h"
#include "isl/id.h"
#include "isl/space.h"
#include "isl/set.h"
#include "isl/map.h"
#include "isl/local_space.h"
#include "isl/aff.h"
#include "isl/polynomial.h"
#include "isl/union_map.h"
#include "isl/union_set.h"
#include "isl/printer.h"
#include "isl/vertices.h"
#include "isl/point.h"
#include "isl/constraint.h"
#include "isl/val.h"
#include "isl/vec.h"
#include "isl/mat.h"
#include "isl/band.h"
#include "isl/schedule.h"
#include "isl/schedule_node.h"
#include "isl/flow.h"
#include "isl/options.h"
#include "isl/ast.h"
#include "isl/ast_build.h"
#include "isl/ilp.h"

#include <unordered_map>
#include <string>
#include <cassert>

namespace isl {

// Forward declares
void lowerCtxRefCount(isl_ctx* aCtx);
void increaseCtxRefCount(isl_ctx* aCtx);

// Forward declares to resolve dependency cycles
""" + "\n".join(["class %s;" % isl_class_to_cpp_class(cls) for cls in CLASSES]) + """

// We have to reference count isl_ctx objects; free it when all objects of the context are freed.
extern std::unordered_map<void*, unsigned> contextUseMap;

template <typename IslObjT>
class IslObjectBase {
protected:
    IslObjT *fData;
    isl_ctx *fCtx;
public:
    // called from copy constructor or plainly
    IslObjectBase(const IslObjT* aData, isl_ctx* aCtx)
        : fData(const_cast<IslObjT*>(aData)) // necessary due to fine constructor matching
        , fCtx(aCtx)
    {
        increaseCtxRefCount(fCtx);
    }

    // called from move constructor
    IslObjectBase(IslObjT* &&aData, isl_ctx* aCtx)
        : fData(aData)
        , fCtx(aCtx)
    {
        aData = nullptr;
    }

    // the derived class destructor has already done its thing
    virtual ~IslObjectBase() = default;

    // don't work with base-class objects
    IslObjectBase(const IslObjectBase<IslObjT> &aOther) = delete;
    IslObjectBase<IslObjT> &operator=(const IslObjectBase<IslObjT> &aOther) = delete;

    IslObjT *release() {
        assert(fData && "cannot release already-released object");
        lowerCtxRefCount(fCtx);
        auto data = fData;
        fData = nullptr;
        return data;
    }

    bool operator==(IslObjectBase<IslObjT> &aOther) {
        return fData == aOther.fData;
    }

    bool operator!=(IslObjectBase<IslObjT> &aOther) {
        return !(this == aOther);
    }
};
"""

CPP_PREAMBLE = """
#include "{header_location}"

namespace isl {{

// Concrete realization of the context map
std::unordered_map<void*, unsigned> contextUseMap;

void lowerCtxRefCount(isl_ctx* aCtx) {{
    contextUseMap[aCtx] -= 1;
    if (!contextUseMap[aCtx]) {{
        contextUseMap.erase(aCtx);
        isl_ctx_free(aCtx);
    }}
}}

void increaseCtxRefCount(isl_ctx* aCtx) {{
    auto it = contextUseMap.find(aCtx);
    contextUseMap[aCtx] = it == contextUseMap.end() ? 1 : it->second + 1;
}}

"""

SAFE_TYPES = list(ENUMS) + ["int", "unsigned", "uint32_t", "size_t", "double",
        "long", "unsigned long"]
SAFE_IN_TYPES = SAFE_TYPES + ["const char *", "char *"]

# {{{ parser

DECL_RE = re.compile(r"""
    (?:__isl_overload\s*)?
    ((?:\w+\s+)*) (\**) \s* (?# return type)
    (\w+) (?# func name)
    \(
    (.*) (?# args)
    \)
    """,
    re.VERBOSE)
FUNC_PTR_RE = re.compile(r"""
    ((?:\w+\s+)*) (\**) \s* (?# return type)
    \(\*(\w+)\) (?# func name)
    \(
    (.*) (?# args)
    \)
    """,
    re.VERBOSE)
STRUCT_DECL_RE = re.compile(
    r"(__isl_export\s+)?"
    "struct\s+"
    "(__isl_subclass\([a-z_ ]+\)\s+)?"
    "([a-z_A-Z0-9]+)\s*;")
ARG_RE = re.compile(r"^((?:\w+)\s+)+(\**)\s*(\w+)$")
INLINE_SEMICOLON_RE = re.compile(r"\;[ \t]*(?=\w)")


def filter_semantics(words):
    semantics = []
    other_words = []
    for w in words:
        if w in ISL_SEM_TO_SEM:
            semantics.append(ISL_SEM_TO_SEM[w])
        else:
            other_words.append(w)

    if semantics:
        assert len(semantics) == 1
        return semantics[0], other_words
    else:
        return None, other_words


def split_at_unparenthesized_commas(s):
    paren_level = 0
    i = 0
    last_start = 0

    while i < len(s):
        c = s[i]
        if c == "(":
            paren_level += 1
        elif c == ")":
            paren_level -= 1
        elif c == "," and paren_level == 0:
            yield s[last_start:i]
            last_start = i+1

        i += 1

    yield s[last_start:i]


class BadArg(ValueError):
    pass


class Retry(ValueError):
    pass


class Undocumented(ValueError):
    pass


class SignatureNotSupported(ValueError):
    pass


def parse_arg(arg):
    if "(*" in arg:
        arg_match = FUNC_PTR_RE.match(arg)
        assert arg_match is not None, "fptr: %s" % arg

        return_semantics, ret_words = filter_semantics(
                arg_match.group(1).split())
        return_decl_words = ret_words[:-1]
        return_base_type = ret_words[-1]

        return_ptr = arg_match.group(2)
        name = arg_match.group(3)
        args = [parse_arg(i.strip())
                for i in split_at_unparenthesized_commas(arg_match.group(4))]

        return CallbackArgument(name.strip(),
                return_semantics,
                return_decl_words,
                return_base_type,
                return_ptr.strip(),
                args)

    words = arg.split()
    semantics, words = filter_semantics(words)

    decl_words = []
    if words[0] in ["struct", "enum"]:
        decl_words.append(words.pop(0))

    rebuilt_arg = " ".join(words)
    arg_match = ARG_RE.match(rebuilt_arg)

    base_type = arg_match.group(1).strip()

    if base_type == "isl_args":
        raise BadArg("isl_args not supported")

    assert arg_match is not None, rebuilt_arg
    return Argument(
            name=arg_match.group(3),
            semantics=semantics,
            decl_words=decl_words,
            base_type=base_type,
            ptr=arg_match.group(2).strip())


class FunctionData:
    def __init__(self, include_dirs):
        self.classes_to_methods = {}
        self.include_dirs = include_dirs
        self.seen_c_names = set()

        self.headers = []

    def read_header(self, fname):
        self.headers.append(fname)

        from os.path import join
        success = False
        for inc_dir in self.include_dirs:
            try:
                inf = open(join(inc_dir, fname), "rt")
            except IOError:
                pass
            else:
                success = True
                break

        if not success:
            raise RuntimeError("header '%s' not found" % fname)

        try:
            lines = inf.readlines()
        finally:
            inf.close()

        # heed continuations, split at semicolons
        new_lines = []
        i = 0
        while i < len(lines):
            my_line = lines[i].strip()
            i += 1

            while my_line.endswith("\\"):
                my_line = my_line[:-1] + lines[i].strip()
                i += 1

            if not my_line.strip().startswith("#"):
                my_line = INLINE_SEMICOLON_RE.sub(";\n", my_line)
                new_lines.extend(my_line.split("\n"))

        lines = new_lines

        i = 0

        while i < len(lines):
            l = lines[i].strip()

            if (not l
                    or l.startswith("extern")
                    or STRUCT_DECL_RE.search(l)
                    or l.startswith("typedef")
                    or l == "}"):
                i += 1
            elif "/*" in l:
                while True:
                    if "*/" in l:
                        i += 1
                        break

                    i += 1

                    l = lines[i].strip()
            elif l.endswith("{"):
                while True:
                    if "}" in l:
                        i += 1
                        break

                    i += 1

                    l = lines[i].strip()

            elif not l:
                i += 1

            else:
                decl = ""

                while True:
                    decl = decl + l
                    if decl:
                        decl += " "
                    i += 1
                    if STRUCT_DECL_RE.search(decl):
                        break

                    open_par_count = sum(1 for i in decl if i == "(")
                    close_par_count = sum(1 for i in decl if i == ")")
                    if open_par_count and open_par_count == close_par_count:
                        break
                    l = lines[i].strip()

                if not STRUCT_DECL_RE.search(decl):
                    self.parse_decl(decl)

    def parse_decl(self, decl):
        decl_match = DECL_RE.match(decl)
        if decl_match is None:
            print("WARNING: func decl regexp not matched: %s" % decl)
            return

        return_base_type = decl_match.group(1)
        return_base_type = return_base_type.replace("ISL_DEPRECATED", "").strip()

        return_ptr = decl_match.group(2)
        c_name = decl_match.group(3)
        args = [i.strip()
                for i in split_at_unparenthesized_commas(decl_match.group(4))]

        if args == ["void"]:
            args = []

        if c_name in [
                "ISL_ARG_DECL",
                "ISL_DECLARE_LIST",
                "ISL_DECLARE_LIST_FN",
                "isl_ast_op_type_print_macro",
                "ISL_DECLARE_MULTI",
                "ISL_DECLARE_MULTI_NEG",
                "ISL_DECLARE_MULTI_DIMS",
                "ISL_DECLARE_MULTI_WITH_DOMAIN",
                "isl_malloc_or_die",
                "isl_calloc_or_die",
                "isl_realloc_or_die",
                "isl_handle_error",
                ]:
            return

        assert c_name.startswith("isl_"), c_name
        name = c_name[4:]

        found_class = False
        for cls in CLASSES:
            if name.startswith(cls):
                found_class = True
                name = name[len(cls)+1:]
                break

        # Don't be tempted to chop off "_val"--the "_val" versions of
        # some methods are incompatible with the isl_int ones.
        #
        # (For example, isl_aff_get_constant() returns just the constant,
        # but isl_aff_get_constant_val() returns the constant divided by
        # the denominator.)
        #
        # To avoid breaking user code in non-obvious ways, the new
        # names are carried over to the Python level.

        if not found_class:
            if name.startswith("options_"):
                found_class = True
                cls = "ctx"
                name = name[len("options_"):]
            elif name.startswith("equality_") or name.startswith("inequality_"):
                found_class = True
                cls = "constraint"
            elif name == "ast_op_type_set_print_name":
                found_class = True
                cls = "printer"
                name = "ast_op_type_set_print_name"

        if name.startswith("2"):
            name = "two_"+name[1:]

        assert found_class, name

        try:
            args = [parse_arg(arg) for arg in args]
        except BadArg:
            print("SKIP: %s %s" % (cls, name))
            return

        if name in CPP_RESERVED_WORDS:
            name = name + "_"

        if cls == "options":
            assert name.startswith("set_") or name.startswith("get_"), (name, c_name)
            name = name[:4]+"option_"+name[4:]

        words = return_base_type.split()

        is_exported = "__isl_export" in words
        if is_exported:
            words.remove("__isl_export")

        is_constructor = "__isl_constructor" in words
        if is_constructor:
            words.remove("__isl_constructor")

        return_semantics, words = filter_semantics(words)
        return_decl_words = []
        if words[0] in ["struct", "enum"]:
            return_decl_words.append(words.pop(0))
        return_base_type = " ".join(words)

        cls_meth_list = self.classes_to_methods.setdefault(cls, [])

        if c_name in self.seen_c_names:
            return

        cls_meth_list.append(Method(
                cls, name, c_name,
                return_semantics, return_decl_words, return_base_type, return_ptr,
                args, is_exported=is_exported, is_constructor=is_constructor))

        self.seen_c_names.add(c_name)

# }}}


# {{{ python wrapper writer

def write_method_prototype(gen, meth):
    if meth.name == "options": return
    gen(meth.cpp_cls())

def write_class(gen, cls_name, methods):
    cpp_cls = isl_class_to_cpp_class(cls_name)
    gen("class {cpp_cls} : public IslObjectBase<isl_{cls}> {{".format(cls=cls_name, cpp_cls=cpp_cls))
    gen("public:")
    with Indentation(gen):
        # Constructor
        gen("""
                {cpp_cls}(isl_{cls} *aData) : IslObjectBase(aData, ctx(aData)) {{}}
            """
            .format(cpp_cls=cpp_cls, cls=cls_name))

        # Alternate constructors, auto-conversions
        gen_conversions(gen, cls_name)
        gen("")

        if cls_name == "ctx":
            gen("""
                isl_ctx *ctx(isl_ctx *aData) {{
                    return aData;
                }}

                ~{cpp_cls}() {{
                    if (fData) {{
                        release();
                    }}
                }}
                """
                .format(cpp_cls=cpp_cls))
            gen("")

        else:
            gen("""
                isl_ctx *ctx(isl_{cls} *aData) {{
                    return isl_{cls}_get_ctx(aData);
                }}

                ~{cpp_cls}() {{
                    if (fData) {{
                        isl_{cls}_free(fData);
                        lowerCtxRefCount(fCtx);
                    }}
                }}
                """
                .format(cls=cls_name, cpp_cls=cpp_cls))
            gen("")

        # Implicit conversion to underlying data type
        gen("""
            operator isl_{cls}*() {{
                return fData;
            }}

            isl_{cls} **operator &() {{
                return &fData;
            }}
        """.format(cls=cls_name))
        gen("")

        if cls_name in NON_COPYABLE:
            # copy constructor and move constructor
            gen("""
            // non-copyable
            {cpp_cls}(const {cpp_cls} &aOther) = delete;

            // but movable
            {cpp_cls}({cpp_cls} &&aOther)
                : IslObjectBase(std::move(aOther.fData), ctx(aOther.fData)) {{}}
            """
            .format(cls=cls_name, cpp_cls=cpp_cls))
            gen("")

            # copy assignment operator and move assignment operator
            gen("""
            {cpp_cls} &operator=(const {cpp_cls} &aOther) = delete;

            {cpp_cls} &operator=({cpp_cls} &&aOther) {{
                fData = aOther.fData;
                aOther.fData = nullptr;
                fCtx = aOther.fCtx;
                return *this;
            }}
            """.format(cls=cls_name, cpp_cls=cpp_cls))
            gen("")
        else:
            # copy constructor and move constructor
            gen("""
                // copyable
                {cpp_cls}(const {cpp_cls} &aOther)
                    : IslObjectBase(aOther.fData, ctx(aOther.fData)) // need to call
                                                                     // lvalue constructor
                {{
                    fData = isl_{cls}_copy(aOther.fData); // do this separately to avoid
                                                          // calling the rvalue constructor
                }}

                // movable too
                {cpp_cls}({cpp_cls} &&aOther)
                    : IslObjectBase(std::move(aOther.fData), ctx(aOther.fData)) {{}}
                """
                .format(cls=cls_name, cpp_cls=cpp_cls))
            gen("")

            # copy assignment operator and move assignment operator
            gen("""
                {cpp_cls} &operator=(const {cpp_cls} &aOther) {{
                    fData = isl_{cls}_copy(aOther.fData);
                    fCtx = aOther.fCtx;
                    increaseCtxRefCount(fCtx);
                    return *this;
                }}

                {cpp_cls} &operator=({cpp_cls} &&aOther) {{
                    fData = aOther.fData;
                    aOther.fData = nullptr;
                    fCtx = aOther.fCtx;
                    return *this;
                }}
                """.format(cls=cls_name, cpp_cls=cpp_cls))
            gen("")

        # method prototypes
        [write_method_prototype(gen, meth) for meth in methods]

    # Closing brace of class
    gen("};")

def gen_conversions(gen, tgt_cls):
    conversions = IMPLICIT_CONVERSIONS.get(tgt_cls, [])
    for src_cls, conversion_method in conversions:
        gen_conversions(gen, src_cls, name)

        gen("""
            {cpp_tgt_cls}({cpp_src_cls} aEl)
                : {cpp_tgt_cls}({cpp_src_cls}.{conversion_method}(aEl)) {{}}
            """
            .format(
                cpp_tgt_cls=isl_class_to_cpp_class(tgt_cls),
                cpp_src_cls=isl_class_to_cpp_class(src_cls),
                conversion_method=conversion_method))


def gen_callback_wrapper(gen, cb, func_name, has_userptr):
    passed_args = []
    input_args = []

    if has_userptr:
        assert cb.args[-1].name == "user"

    pre_call = PythonCodeGenerator()
    post_call = PythonCodeGenerator()
    result_name = "result"

    for arg in cb.args[:-1]:
        if arg.base_type.startswith("isl_") and arg.ptr == "*":
            input_args.append(arg.name)
            passed_args.append("cpp_%s" % arg.name)

            pre_call(
                    "auto cpp_{name} = {cpp_cls}{{{name}}};"
                    .format(
                        name=arg.name,
                        cpp_cls=isl_class_to_cpp_class(arg.base_type)))

            if arg.semantics is SEM_TAKE:
                # We (the callback) are supposed to free the object, so
                # just keep it attached to its wrapper until GC gets
                # rid of it.
                pass
            elif arg.semantics is SEM_KEEP:
                # The caller wants to keep this object, so we'll stop managing
                # it.
                post_call("cpp_{name}.release();".format(name=arg.name))
            else:
                raise SignatureNotSupported(
                        "callback arg semantics not understood: %s" % arg.semantics)

        else:
            raise SignatureNotSupported("unsupported callback arg: %s %s" % (
                arg.base_type, arg.ptr))

    if has_userptr:
        input_args.append("user")
        passed_args.append("nullptr")

    if cb.return_base_type in SAFE_IN_TYPES and cb.return_ptr == "":
        pass

    elif cb.return_base_type.startswith("isl_") and cb.return_ptr == "*":
        ret_cpp_cls = isl_class_to_cpp_class(cb.return_base_type)

        if cb.return_semantics is None:
            raise SignatureNotSupported("callback return with unspecified semantics")
        elif cb.return_semantics is not SEM_GIVE:
            raise SignatureNotSupported("callback return with non-GIVE semantics")

        result_name = "result.release()"

    else:
        raise SignatureNotSupported("unsupported callback signature")

    gen(
            "auto {func_name} = []({input_args}) {{"
            .format(
                func_name=func_name,
                input_args=", ".join([arg.c_declarator() for arg in cb.args])))

    with Indentation(gen):
        gen.extend(pre_call)
        gen(
                "{cpp_ret_type} result = reinterpret_cast<{cb_ctype}>({name})({passed_args});"
                .format(cpp_ret_type=cb.ret_cpp_cls(),
                        cb_ctype=cb.c_declarator_type(), name="user",
                        passed_args=", ".join(passed_args)))

        gen.extend(post_call)
        gen("return {result_name};".format(result_name=result_name))

    gen("};")


def write_method_wrapper(gen, cls_name, meth):
    pre_call = PythonCodeGenerator()

    # There are two post-call phases, "safety", and "check". The "safety"
    # phase's job is to package up all the data returned by the function
    # called. No exceptions may be raised before safety ends.
    #
    # Next, the "check" phase will perform error checking and may raise exceptions.
    safety = PythonCodeGenerator()
    check = PythonCodeGenerator()
    docs = []

    passed_args = []
    input_args = []
    doc_args = []
    ret_val = None
    ret_descr = None

    arg_idx = 0
    while arg_idx < len(meth.args):
        arg = meth.args[arg_idx]

        if isinstance(arg, CallbackArgument):
            has_userptr = (
                    arg_idx + 1 < len(meth.args)
                    and meth.args[arg_idx+1].name == "user")
            if has_userptr:
                arg_idx += 1

            cb_wrapper_name = "cb_wrapper_"+arg.name

            gen_callback_wrapper(pre_call, arg, cb_wrapper_name, has_userptr)

            input_args.append(arg.name)

            passed_args.append(cb_wrapper_name)
            if has_userptr:
                passed_args.append("reinterpret_cast<void*>(%s)" % arg.name)

            docs.append(":param %s: callback(%s) -> %s"
                    % (
                        arg.name,
                        ", ".join(
                            sub_arg.name
                            for sub_arg in arg.args
                            if sub_arg.name != "user"),
                        arg.return_base_type
                        ))

        elif arg.base_type in SAFE_IN_TYPES and not arg.ptr:
            passed_args.append(arg.name)
            input_args.append(arg.name)
            doc_args.append(arg.name)
            doc_cls = arg.base_type
            if doc_cls.startswith("isl_"):
                doc_cls = doc_cls[4:]

            docs.append(":param %s: :class:`%s`" % (arg.name, doc_cls))

        elif arg.base_type in ["char", "const char"] and arg.ptr == "*":
            c_name = "cstr_"+arg.name

            pre_call("auto {c_name} = {arg_name}.c_str();"
                    .format(c_name=c_name, arg_name=arg.name))

            if arg.semantics is SEM_TAKE:
                raise SignatureNotSupported("__isl_take %s %s" % (arg.base_type, arg.ptr))
            else:
                passed_args.append(c_name)
            input_args.append(arg.name)

            docs.append(":param %s: string" % arg.name)

        elif arg.base_type == "int" and arg.ptr == "*":
            if arg.name in ["exact", "tight"]:
                passed_args.append(arg.name)
                docs.append(":param %s: int *" % arg.name)
            else:
                raise SignatureNotSupported("int *")

        elif arg.base_type == "isl_val" and arg.ptr == "*" and arg_idx > 0:
            # {{{ val input argument

            val_name = "val_" + arg.name.replace("*", "")
            fmt_args = dict(
                    arg0_name=meth.args[0].name,
                    name=arg.name,
                    val_name=val_name)

            if arg.semantics is SEM_TAKE:
                # we clone Val so we can release the clone
                pre_call("auto {val_name} = {name};".format(**fmt_args))
                passed_args.append(val_name + ".release()")
            else:
                passed_args.append(arg.name)
            input_args.append(arg.name)

            docs.append(":param %s: :class:`Val`" % arg.name)

            # }}}

        elif arg.base_type.startswith("isl_") and arg.ptr == "*":
            # {{{ isl types input arguments

            arg_cpp_cls = isl_class_to_cpp_class(arg.base_type)

            arg_cls = arg.base_type[4:]
            arg_descr = ":param %s: :class:`%s`" % (
                    arg.name, isl_class_to_cpp_class(arg_cls))

            if arg.semantics is None and arg.base_type != "isl_ctx":
                raise Undocumented(meth)

            copyable = arg_cls not in NON_COPYABLE
            if arg.semantics is SEM_TAKE:
                if copyable:
                    new_name = "copy_"+arg.name.replace("*", "")
                    pre_call('auto {copy_name} = {name};'
                            .format(copy_name=new_name, name=arg.name))
                    arg_descr += " (copied)"
                else:
                    new_name = "move_"+arg.name.replace("*", "")
                    pre_call('auto {move_name} = std::move({name});'
                            .format(move_name=new_name, name=arg.name))
                    arg_descr += " (moved)"

                passed_args.append(new_name+".release()")

            elif arg.semantics is SEM_KEEP or arg.semantics is None:
                passed_args.append(arg.name)

            else:
                raise RuntimeError("unexpected semantics: %s" % arg.semantics)

            input_args.append(arg.name)
            docs.append(arg_descr)

            # }}}

        elif arg.base_type.startswith("isl_") and arg.ptr == "**":
            # {{{ isl types output arguments

            if arg.semantics is not SEM_GIVE:
                raise SignatureNotSupported("non-give secondary ptr return value")

            # Stuff is passed by reference anyway, so the ** is irrelevant to us.
            passed_args.append("&{name}".format(name=arg.name))
            # }}}

        elif (arg.base_type == "void"
                and arg.ptr == "*"
                and arg.name == "user"):

            passed_args.append("nullptr")
            input_args.append(arg.name)

            pre_call("""
                assert(!{name} && "passing non-nullptr arguments for '{name}' is not supported");
                """
                .format(name=arg.name))

            docs.append(":param %s: None" % arg.name)

        else:
            raise SignatureNotSupported("arg type %s %s" % (arg.base_type, arg.ptr))

        arg_idx += 1
        pre_call("")

    # {{{ return value processing

    if meth.return_base_type == "isl_stat" and not meth.return_ptr:
        ret_val = "result"
        ret_descr = "isl_stat"

    elif meth.return_base_type == "isl_bool" and not meth.return_ptr:
        ret_val = "result"
        ret_descr = "isl_bool"

    elif meth.return_base_type in SAFE_TYPES and not meth.return_ptr:
        ret_val = "result"
        ret_descr = meth.return_base_type

    elif (meth.return_base_type.startswith("isl_")
            and meth.return_semantics is SEM_NULL):
        assert not meth.is_mutator

    elif meth.return_base_type.startswith("isl_") and meth.return_ptr == "*":
        ret_cls = meth.return_base_type[4:]

        if meth.is_mutator:
            if ret_val:
                meth.mutator_veto = True
                raise Retry()

            # "this" is consumed, so let's put in a safety check so the destructor doesn't try to
            # double-free us
            safety("fData = nullptr;")
            ret_val = "std::move(result)"
            ret_descr = ":class:`%s` (self)" % isl_class_to_cpp_class(ret_cls)
        else:
            if meth.return_semantics is None and ret_cls != "ctx":
                raise Undocumented(meth)

            if meth.return_semantics is not SEM_GIVE and ret_cls != "ctx":
                raise SignatureNotSupported("non-give return")

            py_ret_cls = isl_class_to_cpp_class(ret_cls)
            ret_val = "result"
            ret_descr = ":class:`%s`" % py_ret_cls

    elif meth.return_base_type in ["const char", "char"] and meth.return_ptr == "*":
        # auto-constructs std::string at return time
        ret_val = "result"
        ret_descr = "string"

    elif (meth.return_base_type == "void"
            and meth.return_ptr == "*"
            and meth.name == "get_user"):

        raise SignatureNotSupported("get_user")

    elif meth.return_base_type == "void" and not meth.return_ptr:
        pass

    else:
        raise SignatureNotSupported("ret type: %s %s in %s" % (
            meth.return_base_type, meth.return_ptr, meth))

    # }}}

    check("")
    if not ret_val:
        ret_descr = "(nothing)"

    else:
        check("return " + ret_val + ";")
        ret_descr = ret_descr

    docs = (["%s(%s)" % (meth.name, ", ".join(input_args)), ""]
            + docs
            + [":return: %s" % ret_descr])

    cpp_cls = isl_class_to_cpp_class(cls_name)

    # doxygen comments
    gen('\n'.join(["/// " + doc for doc in [""] + docs]))
    gen("{ret_cpp_cls} {cpp_cls}::{name}({input_args}) {{"
            .format(ret_cpp_cls=meth.ret_cpp_cls(), cpp_cls=cpp_cls,
                    name=meth.name, input_args=meth.prune_this().args_cpp_cls()))

    gen.indent()
    gen("")
    gen.extend(pre_call)
    gen("")

    if ret_val:
        gen("{ret_cpp_cls} result = {c_name}({args});"
            .format(ret_cpp_cls=meth.ret_cpp_cls(), c_name=meth.c_name, args=", ".join(passed_args)))
    else:
        gen("{c_name}({args});".format(c_name=meth.c_name, args=", ".join(passed_args)))

    gen.extend(safety)
    gen.extend(check)
    gen.dedent()
    gen("}")
    gen("")

# }}}


ADD_VERSIONS = {
        "union_pw_aff": 15,
        "multi_union_pw_aff": 15,
        "basic_map_list": 15,
        "map_list": 15,
        "union_set_list": 15,
        }


def gen_wrapper(include_dirs, include_barvinok=False, isl_version=None,
                wrapper_header_location="Isl.hpp"):
    fdata = FunctionData(["."] + include_dirs)
    fdata.read_header("isl/ctx.h")
    fdata.read_header("isl/id.h")
    fdata.read_header("isl/space.h")
    fdata.read_header("isl/set.h")
    fdata.read_header("isl/map.h")
    fdata.read_header("isl/local_space.h")
    fdata.read_header("isl/aff.h")
    fdata.read_header("isl/polynomial.h")
    fdata.read_header("isl/union_map.h")
    fdata.read_header("isl/union_set.h")
    fdata.read_header("isl/printer.h")
    fdata.read_header("isl/vertices.h")
    fdata.read_header("isl/point.h")
    fdata.read_header("isl/constraint.h")
    fdata.read_header("isl/val.h")
    fdata.read_header("isl/vec.h")
    fdata.read_header("isl/mat.h")
    fdata.read_header("isl/band.h")
    fdata.read_header("isl/schedule.h")
    fdata.read_header("isl/schedule_node.h")
    fdata.read_header("isl/flow.h")
    fdata.read_header("isl/options.h")
    fdata.read_header("isl/ast.h")
    fdata.read_header("isl/ast_build.h")
    fdata.read_header("isl/ilp.h")

    if isl_version is None:
        fdata.read_header("isl_declaration_macros_expanded.h")
    else:
        fdata.read_header("isl_declaration_macros_expanded_v%d.h"
                % isl_version)
    fdata.headers.pop()

    undoc = []

    with open("islpy/Isl.hpp", "wt") as wrapper_f:
        wrapper_gen = PythonCodeGenerator()

        wrapper_f.write(
                "// AUTOMATICALLY GENERATED by gen_wrap.py -- do not edit\n")
        wrapper_f.write(HEADER_PREAMBLE)

        wrapper_gen("// {{{ classes")

        for cls_name in CLASSES:
            methods = fdata.classes_to_methods.get(cls_name, [])
            methods = [meth for meth in methods if meth.c_name not in DEPRECATED_METHODS]

            # First, write the class with prototypes of member functions.
            write_class(wrapper_gen, cls_name, methods)
            wrapper_gen("")

        wrapper_gen("// }}}")
        wrapper_gen("} // end namespace isl")
        wrapper_gen("")

        wrapper_f.write("\n" + wrapper_gen.get())

    with open("islpy/Isl.cpp", "wt") as wrapper_f:
        wrapper_gen = PythonCodeGenerator()

        wrapper_f.write(
                "// AUTOMATICALLY GENERATED by gen_wrap.py -- do not edit\n")
        wrapper_f.write(CPP_PREAMBLE.format(header_location=wrapper_header_location))

        for cls_name in CLASSES:
            methods = fdata.classes_to_methods.get(cls_name, [])
            methods = [meth for meth in methods if meth.c_name not in DEPRECATED_METHODS]

            # Then define all the methods.
            wrapper_gen("// {{{ methods of " + cls_name)
            wrapper_gen("")

            for meth in methods:
                if meth.name in ["free", "set_free_user"]:
                    continue

                try:
                    write_method_wrapper(wrapper_gen, cls_name, meth)
                except Retry:
                    write_method_wrapper(wrapper_gen, cls_name, meth)
                except Undocumented:
                    undoc.append(str(meth))
                except SignatureNotSupported:
                    _, e, _ = sys.exc_info()
                    print("SKIP (sig not supported: %s): %s" % (e, meth))
                else:
                    pass

            wrapper_gen("// }}}")
            wrapper_gen("")

        wrapper_gen("// }}}")
        wrapper_gen("} // end namespace isl")
        wrapper_gen("")

        wrapper_f.write("\n" + wrapper_gen.get())

    print("SKIP (%d undocumented methods): %s" % (len(undoc), ", ".join(undoc)))

    return fdata.headers

if __name__ == "__main__":
    from os.path import expanduser
    gen_wrapper([expanduser("isl/include")])

# vim: foldmethod=marker