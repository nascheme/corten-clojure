//! Kernel primitives.
//!
//! A primitive is a Rust function taking its arguments as a slice. KFunction
//! objects store the primitive's **index** in this table as a fixnum in
//! `FUNCTION_ADDRESS`, never a raw Rust pointer, so heap images survive
//! kernel rebuilds. Each is the value of a var of namespace
//! `corten.kernel` (`clj::init`); the compiler calls those directly
//! (`compiler/frontend/clj.rs`), other calls go through the funcall stub
//! (`runtime.rs`).

pub mod core;
pub mod kernel;

use crate::heap;
use crate::object::*;
use self::core as c;
use self::kernel as k;

pub type PrimFn = fn(&[LispObj]) -> LispObj;

/// A kernel function: its name in `corten.kernel`, its argument count
/// range (checked by `call_prim`, and at compile time for direct calls)
/// and its implementation.
pub struct Prim {
    pub name: &'static str,
    pub min: usize,
    pub max: usize,
    pub func: PrimFn,
}

const MAX: usize = usize::MAX;

macro_rules! prim {
    ($name:literal, $min:expr, $max:expr, $func:path) => {
        Prim { name: $name, min: $min, max: $max, func: $func }
    };
}

pub static PRIMS: &[Prim] = &[
    prim!("funcall", 1, MAX, c::funcall_prim),
    prim!("progv", 3, 3, crate::runtime::progv_prim),
    prim!("apply", 2, MAX, crate::clj::coll::apply_prim),
    prim!("nil?", 1, 1, c::null_prim),
    prim!("not", 1, 1, crate::clj::prims::not_prim),
    prim!("eq", 2, 2, c::eq_prim),
    prim!("string?", 1, 1, c::stringp),
    prim!("integer?", 1, 1, c::integerp),
    prim!("char?", 1, 1, c::characterp),
    prim!("fn?", 1, 1, c::functionp),
    prim!("+", 0, MAX, c::plus),
    prim!("-", 1, MAX, c::minus),
    prim!("*", 0, MAX, c::times),
    prim!("/", 1, MAX, c::divide),
    prim!("=", 1, MAX, c::num_eq),
    prim!("<", 1, MAX, c::num_lt),
    prim!("<=", 1, MAX, c::num_le),
    prim!(">", 1, MAX, c::num_gt),
    prim!(">=", 1, MAX, c::num_ge),
    prim!("truncate", 1, 2, c::truncate),
    prim!("mod", 2, 2, c::mod_prim),
    prim!("flush", 0, 0, crate::clj::host::flush),
    prim!("get-internal-real-time", 0, 0, c::get_internal_real_time),
    prim!("char-int", 1, 1, k::char_int),
    prim!("int-char", 1, 1, k::int_char),
    prim!("number?", 1, 1, k::numberp),
    prim!("string=", 2, 2, k::string_equal),
    prim!("close", 1, 1, crate::clj::host::close),
    prim!("open-input-file", 1, 1, crate::clj::host::open_input_file),
    prim!("float", 1, 2, k::float),
    prim!("gc", 0, 1, k::gc),
    prim!("svref", 2, 2, k::svref),
    prim!("read", 1, 4, crate::clj::reader::read_prim),
    prim!("read-string", 1, 2, crate::clj::reader::read_string_prim),
    prim!("form-source", 2, 2, crate::clj::reader::form_source),
    prim!("simple-vector?", 1, 1, crate::clj::host::simple_vector_p),
    prim!("length", 1, 1, crate::clj::host::length),
    prim!("type?", 1, 1, crate::clj::host::type_p),
    prim!("var-name", 1, 1, crate::clj::host::var_name_prim),
    prim!("subseq", 2, 3, crate::clj::host::subseq),
    prim!("char", 2, 2, crate::clj::host::char_at),
    prim!("reverse", 1, 1, crate::clj::host::reverse),
    prim!("string-index-of", 2, 3, crate::clj::host::string_index_of),
    prim!("string-concat", 0, MAX, crate::clj::host::string_concat),
    prim!("var-ns", 1, 1, crate::clj::host::var_ns_prim),
    prim!("var-meta", 1, 1, crate::clj::host::var_meta_prim),
    prim!("ns-name", 1, 1, crate::clj::host::ns_name_prim),
    prim!("max", 1, MAX, crate::clj::host::max),
    prim!("min", 1, MAX, crate::clj::host::min),
    prim!("write-string", 1, 2, crate::clj::host::write_string),
    prim!("string-input-stream", 1, 1, crate::clj::host::string_input_stream),
    prim!("clear-input", 0, 1, crate::clj::host::clear_input),
    prim!("read-char", 1, 1, crate::clj::host::read_char),
    prim!("unread-char", 1, 1, crate::clj::host::unread_char),
    prim!("read-line", 1, 1, crate::clj::host::read_line),
    prim!("js-obj", 0, MAX, crate::clj::js::js_obj),
    prim!("js-object?", 1, 1, crate::clj::js::js_object_p),
    prim!("oget", 2, 3, crate::clj::js::oget),
    prim!("oset", 3, 3, crate::clj::js::oset),
    prim!("ocontains", 2, 2, crate::clj::js::ocontains),
    prim!("okeys", 1, 1, crate::clj::js::okeys),
    prim!("oclone", 1, 1, crate::clj::js::oclone),
    prim!("odelete", 2, 2, crate::clj::js::odelete),
    prim!("regexp", 1, 2, crate::clj::js::regexp),
    prim!("regexp?", 1, 1, crate::clj::js::regexp_p),
    prim!("uid", 1, 1, crate::clj::js::uid),
    prim!("js-lt", 2, 2, crate::clj::js::js_lt),
    prim!("astable-sort", 2, 2, crate::clj::js::astable_sort),
    prim!("random", 0, 0, crate::clj::js::random),
    prim!("ashuffle", 1, 1, crate::clj::js::ashuffle),
    prim!("now-ms", 0, 0, crate::clj::js::now_ms),
    prim!("zero?", 1, 1, crate::clj::js::zerop),
    prim!("math-floor", 1, 1, crate::clj::js::math_floor),
    prim!("math-ceil", 1, 1, crate::clj::js::math_ceil),
    prim!("parse-int", 2, 2, crate::clj::js::parse_int),
    prim!("var-meta-put", 3, 3, crate::clj::js::var_meta_put),
    prim!("string-trim", 2, 2, crate::clj::host::string_trim),
    prim!("object-is", 2, 2, crate::clj::host::object_is),
    prim!("safe-integer?", 1, 1, crate::clj::host::safe_integer_p),
    prim!("math-fn", 2, 2, crate::clj::host::math_fn),
    prim!("math-pow", 2, 2, crate::clj::host::math_pow),
    prim!("math1", 2, 2, crate::clj::math::math1),
    prim!("math2", 3, 3, crate::clj::math::math2),
    prim!("math-exact", 2, 3, crate::clj::math::math_exact),
    prim!("double-compare", 2, 2, crate::clj::math::double_compare),
    prim!("format", 3, 3, crate::clj::format::format),
    prim!("format-timestamp", 7, 7, crate::clj::host::format_timestamp),
    prim!("throw", 1, 1, crate::clj::exceptions::throw),
    prim!("instance?", 2, 2, crate::clj::exceptions::instance_p),
    prim!("kernel-type", 1, 1, crate::clj::exceptions::kernel_type),
    prim!("set-super", 2, 2, crate::clj::exceptions::set_super),
    prim!("type-super", 1, 1, crate::clj::exceptions::type_super_prim),
    prim!("sq-map", 1, 1, crate::clj::reader::sq_map_prim),
    prim!("first", 1, 1, crate::clj::coll::first_prim),
    prim!("next", 1, 1, crate::clj::coll::next_prim),
    prim!("rest", 1, 1, crate::clj::coll::rest_prim),
    prim!("seq", 1, 1, crate::clj::coll::seq_prim),
    prim!("count", 1, 1, crate::clj::coll::count_prim),
    prim!("nth", 2, 3, crate::clj::coll::nth_prim),
    prim!("cons", 2, 2, crate::clj::coll::cons_prim),
    prim!("conj", 2, 2, crate::clj::coll::conj_prim),
    prim!("assoc", 3, 3, crate::clj::coll::assoc_prim),
    prim!("get", 2, 3, crate::clj::coll::get_prim),
    prim!("contains?", 2, 2, crate::clj::coll::contains_prim),
    prim!("meta", 1, 1, crate::clj::coll::meta_prim),
    prim!("with-meta", 2, 2, crate::clj::coll::with_meta_prim),
    prim!("equiv", 2, 2, crate::clj::coll::equal_prim),
    prim!("list", 0, MAX, crate::clj::coll::list_prim),
    prim!("vector", 0, MAX, crate::clj::coll::vector_prim),
    prim!("vec", 1, 1, crate::clj::coll::vec_prim),
    prim!("array-map", 0, MAX, crate::clj::coll::array_map_prim),
    prim!("hash-set", 0, MAX, crate::clj::coll::hash_set_prim),
    prim!("seq?", 1, 1, crate::clj::coll::seq_p),
    prim!("vector?", 1, 1, crate::clj::coll::vector_p),
    prim!("map?", 1, 1, crate::clj::coll::map_p),
    prim!("set?", 1, 1, crate::clj::coll::set_p),
    prim!("concat", 0, MAX, crate::clj::coll::concat_prim),
    prim!("sq-set", 1, 1, crate::clj::reader::sq_set_prim),
    prim!("pr-str", 1, 3, crate::clj::printer::pr_str_prim),
    prim!("bit", 3, 3, crate::clj::prims::bit_op),
    prim!("arith", 4, 4, crate::clj::number::arith),
    prim!("lbit", 3, 3, crate::clj::number::lbit),
    prim!("lbit-not", 1, 1, crate::clj::number::lbit_not),
    prim!("num-equiv", 2, 2, crate::clj::number::num_equiv),
    prim!("hash-number", 1, 1, crate::clj::number::hash_number),
    prim!("num-kind", 1, 1, crate::clj::number::num_kind),
    prim!("num-cast", 2, 2, crate::clj::number::num_cast),
    prim!("integer-string", 2, 2, crate::clj::number::integer_string),
    prim!("parse-integer", 2, 2, crate::clj::number::parse_integer),
    prim!("int32", 1, 1, crate::clj::prims::int32_prim),
    prim!("quot", 2, 2, crate::clj::prims::quot_prim),
    prim!("rem", 2, 2, crate::clj::prims::rem_prim),
    prim!("bit-not", 1, 1, crate::clj::prims::bit_not),
    prim!("aget", 2, 2, crate::clj::array::aget),
    prim!("aset", 3, 3, crate::clj::array::aset),
    prim!("alength", 1, 1, crate::clj::array::alength),
    prim!("make-array", 1, 1, crate::clj::array::make_array),
    prim!("array", 0, MAX, crate::clj::array::array),
    prim!("array?", 1, 1, crate::clj::array::array_p),
    prim!("typed-array", 2, 2, crate::clj::array::typed_array),
    prim!("array-kind", 1, 1, crate::clj::array::array_kind),
    prim!("apush", 2, MAX, crate::clj::array::apush),
    prim!("apop", 1, 1, crate::clj::array::apop),
    prim!("aslice", 1, 3, crate::clj::array::aslice),
    prim!("asplice", 3, 3, crate::clj::array::asplice),
    prim!("acopy", 5, 5, crate::clj::array::acopy),
    prim!("regex-exec", 4, 4, crate::clj::regex::regex_exec),
    prim!("regex-group-count", 2, 2, crate::clj::regex::regex_group_count),
    prim!("hash-double", 1, 1, crate::clj::prims::hash_double),
    prim!("set-hook", 2, 2, crate::clj::prims::set_hook),
    prim!("add-load-path", 1, 1, crate::clj::prims::add_load_path),
    prim!("set-reader-features", 0, MAX, crate::clj::prims::set_reader_features),
    prim!("require", 1, 2, crate::clj::prims::require),
    prim!("make-protocol", 2, 2, crate::clj::prims::make_protocol),
    prim!("protocol-method-id", 2, 2, crate::clj::prims::protocol_method_id),
    prim!("protocol-marker", 1, 1, crate::clj::prims::protocol_marker),
    prim!("protocol?", 1, 1, crate::clj::prims::protocol_p),
    prim!("case-equal", 2, 2, crate::clj::prims::case_equal),
    prim!("symbol?", 1, 1, crate::clj::prims::symbol_p),
    prim!("keyword?", 1, 1, crate::clj::prims::keyword_p),
    prim!("boolean?", 1, 1, crate::clj::prims::boolean_p),
    prim!("name", 1, 1, crate::clj::prims::name_prim),
    prim!("namespace", 1, 1, crate::clj::prims::namespace_prim),
    prim!("symbol-meta", 1, 1, crate::clj::prims::symbol_meta),
    prim!("gensym", 0, 1, crate::clj::prims::gensym),
    prim!("symbol-equal", 2, 2, crate::clj::prims::symbol_equal),
    prim!("str", 1, 1, crate::clj::prims::str_prim),
    prim!("type-of", 1, 1, crate::clj::prims::type_of_prim),
    prim!("type-name", 1, 1, crate::clj::prims::type_name_prim),
    prim!("js-fn?", 1, 1, crate::clj::prims::js_fn_p),
    prim!("builtin-type", 1, 1, crate::clj::prims::builtin_type),
    prim!("new-method-id", 1, 1, crate::clj::prims::new_method_id_prim),
    prim!("extend", 3, 3, crate::clj::prims::extend),
    prim!("method", 2, 2, crate::clj::prims::method),
    prim!("has-method", 2, 2, crate::clj::prims::has_method),
    prim!("type-has-method", 2, 2, crate::clj::prims::type_has_method),
    prim!("set-invoke-id", 1, 1, crate::clj::prims::set_invoke_id),
    prim!("add-obj-method", 3, 3, crate::clj::prims::add_obj_method_prim),
    prim!("invoke-method", 2, MAX, crate::clj::prims::invoke_method),
    prim!("call-method", 3, MAX, crate::clj::prims::call_method),
    prim!("get-field", 2, 2, crate::clj::prims::get_field),
    prim!("put-field", 3, 3, crate::clj::prims::put_field),
    prim!("new", 1, MAX, crate::clj::prims::new_instance),
    prim!("def", 2, 2, crate::clj::prims::def),
    prim!("arity-error", 2, 2, crate::clj::prims::arity_error),
    prim!("eval", 1, 1, crate::clj::prims::eval),
    prim!("load", 1, 1, crate::clj::prims::load),
    prim!("in-ns", 1, 2, crate::clj::prims::in_ns),
    prim!("refer", 2, 3, crate::clj::prims::refer),
    prim!("ns-entries", 1, 1, crate::clj::prims::ns_entries),
    prim!("div", 2, 2, crate::clj::prims::div),
    prim!("parse-float", 1, 1, crate::clj::prims::parse_float),
    prim!("parse-timestamp", 1, 1, crate::clj::prims::parse_timestamp_prim),
    prim!("var?", 1, 1, crate::clj::prims::var_p),
    prim!("var-invoke", 1, MAX, crate::clj::prims::var_invoke),
    prim!("var-get", 1, 1, crate::clj::prims::var_get),
    prim!("var-bound?", 1, 1, crate::clj::prims::var_bound_p),
    prim!("var-root", 1, 1, crate::clj::prims::var_root_prim),
    prim!("var-set-root", 2, 2, crate::clj::prims::var_set_root),
    prim!("var-unbind-root", 1, 1, crate::clj::prims::var_unbind_root),
    prim!("thread-bindings", 0, 0, crate::clj::prims::thread_bindings_prim),
    prim!("var-thread-bound?", 1, 1, crate::clj::prims::var_thread_bound_p),
    prim!("var-dynamic", 1, 2, crate::clj::prims::var_dynamic),
    prim!("var-set-meta", 2, 2, crate::clj::prims::var_set_meta),
    prim!("ns-unmap", 2, 2, crate::clj::prims::ns_unmap),
    prim!("ns-unalias", 2, 2, crate::clj::prims::ns_unalias),
    prim!("ns-aliases", 1, 1, crate::clj::prims::ns_aliases_prim),
    prim!("remove-ns", 1, 1, crate::clj::prims::remove_ns_prim),
    prim!("add-loaded-lib", 1, 1, crate::clj::prims::add_loaded_lib_prim),
    prim!("loaded-libs", 0, 0, crate::clj::prims::loaded_libs),
    prim!("load-resource", 1, 1, crate::clj::prims::load_resource),
    prim!("implements", 2, 2, crate::clj::prims::implements),
    prim!("utc-part", 2, 2, crate::clj::prims::utc_part),
    prim!("identical?", 2, 2, crate::clj::prims::identical_p),
    prim!("refer-name", 3, 4, crate::clj::prims::refer_name),
    prim!("refer-var", 2, 2, crate::clj::prims::refer_var_prim),
    prim!("alias", 3, 3, crate::clj::prims::alias),
    prim!("find-ns", 1, 1, crate::clj::prims::find_ns_prim),
    prim!("all-ns", 0, 0, crate::clj::prims::all_ns),
    prim!("resolve-ns", 1, 1, crate::clj::prims::resolve_ns_prim),
    prim!("require-macros", 1, 2, crate::clj::prims::require_macros),
    prim!("create-ns", 1, 1, crate::clj::prims::create_ns_prim),
    prim!("intern", 2, 2, crate::clj::prims::intern),
    prim!("resolve", 1, 1, crate::clj::prims::resolve_prim),
    prim!("macroexpand-1", 1, 1, crate::clj::prims::macroexpand_1),
    prim!("set-macro", 2, 2, crate::clj::prims::set_macro),
    prim!("symbol", 1, 2, crate::clj::prims::symbol_prim),
    prim!("keyword", 1, 2, crate::clj::prims::keyword_prim),
    prim!("symbol-with-meta", 2, 2, crate::clj::prims::symbol_with_meta),
    prim!("set-global", 2, 2, crate::clj::prims::set_global_prim),
    prim!("global", 1, 1, crate::clj::prims::global_prim),
    prim!("instance-type", 1, 1, crate::clj::prims::instance_type_prim),
    prim!("make-instance", 1, MAX, crate::clj::prims::make_instance_prim),
    prim!("make-type", 2, 2, crate::clj::prims::make_type_prim),
    prim!("field", 2, 2, crate::clj::prims::field),
    prim!("with-field", 3, 3, crate::clj::prims::with_field),
    prim!("specify", 1, 1, crate::clj::prims::specify),
    prim!("set-field", 3, 3, crate::clj::prims::set_field),
    prim!("boolean", 1, 1, crate::clj::prims::boolean_prim),
];

/// A kernel function object for `PRIMS` (or native) entry `index`.
pub fn kernel_function_node(index: usize) -> LispObj {
    let f = heap::alloc_uvector(KFUNCTION_SIZE, KFUNCTION_TYPE);
    f.uset(FUNCTION_ENVIRONMENT, NIL);
    f.uset(FUNCTION_ADDRESS, LispObj::fixnum(index as i64));
    f
}

/// Index of the kernel function `name` in `PRIMS`.
pub fn prim_index(name: &str) -> usize {
    static INDEX: std::sync::OnceLock<std::collections::HashMap<&'static str, usize>> =
        std::sync::OnceLock::new();
    let m = INDEX.get_or_init(|| PRIMS.iter().enumerate().map(|(i, p)| (p.name, i)).collect());
    *m.get(name).unwrap_or_else(|| panic!("no kernel function {name}"))
}

/// Call kernel function `idx`: a `PRIMS` entry, then the native
/// functions (`clj::native`).
#[inline]
pub fn call_prim(idx: usize, a: &[LispObj]) -> LispObj {
    match PRIMS.get(idx) {
        Some(p) => {
            if a.len() < p.min || a.len() > p.max {
                crate::error::arity_error(a.len(), &format!("corten.kernel/{}", p.name));
            }
            (p.func)(a)
        }
        None => crate::clj::native::call(idx - PRIMS.len(), a),
    }
}

/// Call a function object.
///
/// Kernel functions are also called through `lisp_call`, which copies the
/// arguments to the stack: `args` may be in a Rust `Vec`, which is not a
/// GC root, and the primitive may allocate.
pub fn funcall(f: LispObj, args: &[LispObj]) -> LispObj {
    if crate::runtime::is_funcallable(f) {
        crate::runtime::lisp_call(f, args)
    } else {
        crate::error::type_error(format!("Not a function: {}", crate::clj::printer::error_str(f)))
    }
}
