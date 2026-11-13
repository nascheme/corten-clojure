//! Object representation.
//!
//! Low 3 bits are the tag. Fixnums are `n << 3` (61-bit). Characters are
//! `code << 8 | ImmediateTag`. Heap objects are uvectors, 16-byte
//! aligned; a uvector pointer is the address + 5 and cell 0 holds the header
//! `(length_in_16_byte_units << 8) | (type << 3) | UvectorLengthTag`.


use std::fmt;

/// A tagged Lisp value. One machine word.
#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct LispObj(pub usize);

impl fmt::Debug for LispObj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "LispObj({:#x})", self.0)
    }
}

pub const CELL: usize = 8;

// Tags
pub const FIXNUM_TAG: usize = 0;
pub const IMMEDIATE_TAG: usize = 1;
/// GC forwarding pointer (in the first word of a moved object).
pub const FORWARD_TAG: usize = 2;
pub const UVECTOR_TAG: usize = 5;
pub const UVECTOR_LENGTH_TAG: usize = 6;

// Immediate types
pub const CHARACTER_TYPE: usize = 0;
pub const NIL_TYPE: usize = 2;
pub const BOOLEAN_TYPE: usize = 3;
/// The unbound marker (never a value of a program).
pub const UNBOUND_TYPE: usize = 4;

/// `nil`, `false` and `true` are immediates. `nil` and `false` differ
/// only in bit `FALSY_BIT`, so a value is falsy when
/// `x | FALSY_BIT == FALSE`.
pub const NIL: LispObj = LispObj((NIL_TYPE << 3) | IMMEDIATE_TAG);
pub const FALSE: LispObj = LispObj((BOOLEAN_TYPE << 3) | IMMEDIATE_TAG);
pub const TRUE: LispObj = LispObj((1 << 8) | (BOOLEAN_TYPE << 3) | IMMEDIATE_TAG);
pub const FALSY_BIT: usize = NIL.0 ^ FALSE.0;
const _: () = assert!(FALSY_BIT.is_power_of_two() && NIL.0 | FALSY_BIT == FALSE.0);

/// Clojure boolean from a Rust bool.
#[inline]
pub const fn cbool(b: bool) -> LispObj {
    if b { TRUE } else { FALSE }
}

/// Clojure truthiness: anything but `nil` and `false`.
#[inline]
pub const fn truthy(x: LispObj) -> bool {
    x.0 | FALSY_BIT != FALSE.0
}

#[inline]
pub const fn is_boolean(x: LispObj) -> bool {
    x.0 == FALSE.0 || x.0 == TRUE.0
}

// Uvector types. Function types must stay below the others
// (`is_function`).
pub const FUNCTION_TYPE: usize = 0;
pub const KFUNCTION_TYPE: usize = 1;
pub const ARRAY_TYPE: usize = 3;
pub const DOUBLE_FLOAT_TYPE: usize = 6;
pub const COMPILED_CODE_TYPE: usize = 10;
pub const RATIO_TYPE: usize = 13;
pub const BIGNUM_TYPE: usize = 14;
pub const SIMPLE_VECTOR_TYPE: usize = 17;
pub const SIMPLE_CHAR_VECTOR_TYPE: usize = 18;
pub const SINGLE_FLOAT_TYPE: usize = 24;
/// Clojure object (see `clj/mod.rs`): cell 1 is its type descriptor.
pub const CLJ_INSTANCE_TYPE: usize = 28;

// Function / KFunction
pub const FUNCTION_ENVIRONMENT: usize = 1;
pub const FUNCTION_ADDRESS: usize = 2;
pub const FUNCTION_SIZE: usize = 3;
pub const KFUNCTION_SIZE: usize = 3;

// Arrays
pub const ARRAY_VECTOR: usize = 1;
pub const ARRAY_FILL_POINTER: usize = 2;
pub const ARRAY_OFFSET: usize = 3;
pub const ARRAY_DIMENSIONS: usize = 4;
pub const ARRAY_DIM1_LENGTH: usize = 5;
pub const ARRAY_SIMPLE_VECTOR_LENGTH: usize = 1;
pub const ARRAY_SIMPLE_VECTOR_START: usize = 2;

/// The value of an unbound var.
pub const SYMBOL_UNBOUND: LispObj = LispObj((UNBOUND_TYPE << 3) | IMMEDIATE_TAG);

// Double float: header and value, 16 bytes.
pub const DOUBLE_FLOAT_OFFSET: usize = 1;
pub const DOUBLE_FLOAT_SIZE: usize = 1;
pub const SINGLE_FLOAT_OFFSET: usize = 1;

/// Element type of character vectors (UTF-16 code unit).
pub type LispChar = u16;

/// The UTF-16 units of simple string `str`.
pub fn units_of(str: LispObj) -> &'static [LispChar] {
    let len = str.vector_length().integer() as usize;
    unsafe { std::slice::from_raw_parts(str.char_array_start(), len) }
}

/// Whether simple strings `a` and `b` have the same contents.
pub fn lisp_strings_equal(a: LispObj, b: LispObj) -> bool {
    units_of(a) == units_of(b)
}

pub const FIXNUM_BITS: u32 = 61;
pub const MOST_POSITIVE_FIXNUM: i64 = (1i64 << (FIXNUM_BITS - 1)) - 1;
pub const MOST_NEGATIVE_FIXNUM: i64 = -MOST_POSITIVE_FIXNUM - 1;

impl LispObj {
    #[inline]
    pub const fn raw(self) -> usize {
        self.0
    }
    #[inline]
    pub const fn tag(self) -> usize {
        self.0 & 7
    }
    #[inline]
    pub const fn strip_tag(self) -> usize {
        self.0 & !7
    }

    // ---- fixnums ----
    #[inline]
    pub const fn fixnum(n: i64) -> LispObj {
        LispObj((n << 3) as usize)
    }
    #[inline]
    pub fn fixnum_checked(n: i64) -> Option<LispObj> {
        (MOST_NEGATIVE_FIXNUM..=MOST_POSITIVE_FIXNUM)
            .contains(&n)
            .then(|| LispObj::fixnum(n))
    }
    #[inline]
    pub const fn is_fixnum(self) -> bool {
        self.0 & 7 == FIXNUM_TAG
    }
    /// Value of a fixnum (arithmetic shift).
    #[inline]
    pub const fn integer(self) -> i64 {
        (self.0 as i64) >> 3
    }

    // ---- characters / immediates ----
    #[inline]
    pub const fn character(code: u32) -> LispObj {
        LispObj(((code as usize) << 8) | IMMEDIATE_TAG)
    }
    #[inline]
    pub const fn is_immediate(self) -> bool {
        self.tag() == IMMEDIATE_TAG
    }
    #[inline]
    pub const fn immediate_type(self) -> usize {
        (self.0 >> 3) & 0x1f
    }
    #[inline]
    pub const fn is_character(self) -> bool {
        self.is_immediate() && self.immediate_type() == CHARACTER_TYPE
    }
    #[inline]
    pub const fn char_code(self) -> u32 {
        (self.0 >> 8) as u32
    }

    // ---- heap objects ----
    #[inline]
    pub const fn is_uvector(self) -> bool {
        self.tag() == UVECTOR_TAG
    }
    #[inline]
    pub const fn is_uvector_header(self) -> bool {
        self.tag() == UVECTOR_LENGTH_TAG
    }


    /// Pointer to cell 0 (the header) of a uvector.
    #[inline]
    pub fn uvector_ptr(self) -> *mut LispObj {
        debug_assert!(self.is_uvector());
        (self.0 - UVECTOR_TAG) as *mut LispObj
    }
    /// Read cell `i` of a uvector (cell 0 = header).
    #[inline]
    pub fn uref(self, i: usize) -> LispObj {
        unsafe { *self.uvector_ptr().add(i) }
    }
    #[inline]
    pub fn uset(self, i: usize, v: LispObj) {
        unsafe { *self.uvector_ptr().add(i) = v }
    }
    #[inline]
    pub fn uvector_type(self) -> usize {
        (self.uref(0).0 >> 3) & 0x1f
    }
    /// Length in 16-byte units.
    #[inline]
    pub fn uvector_size(self) -> usize {
        self.uref(0).0 >> 8
    }
    #[inline]
    pub fn set_uvector_type(self, ty: usize) {
        let h = self.uref(0).0;
        self.uset(0, LispObj((h & !0xf8) | (ty << 3)));
    }
    #[inline]
    pub fn is_uvector_of(self, ty: usize) -> bool {
        self.is_uvector() && self.uvector_type() == ty
    }

    pub fn is_double_float(self) -> bool {
        self.is_uvector_of(DOUBLE_FLOAT_TYPE)
    }
    pub fn is_function(self) -> bool {
        self.is_uvector() && self.uvector_type() <= KFUNCTION_TYPE
    }
    pub fn is_kfunction(self) -> bool {
        self.is_uvector_of(KFUNCTION_TYPE)
    }
    pub fn is_adjustable_array(self) -> bool {
        self.is_uvector_of(ARRAY_TYPE)
    }
    pub fn is_simple_vector(self) -> bool {
        self.is_uvector() && matches!(self.uvector_type(), SIMPLE_VECTOR_TYPE | SIMPLE_CHAR_VECTOR_TYPE)
    }
    pub fn is_simple_char_vector(self) -> bool {
        self.is_uvector_of(SIMPLE_CHAR_VECTOR_TYPE)
    }
    pub fn is_simple_generic_vector(self) -> bool {
        self.is_uvector_of(SIMPLE_VECTOR_TYPE)
    }
    pub fn is_array(self) -> bool {
        self.is_simple_vector() || self.is_adjustable_array()
    }
    pub fn array_dimensions(self) -> usize {
        if self.is_adjustable_array() {
            self.uref(ARRAY_DIMENSIONS).integer() as usize
        } else {
            1
        }
    }
    pub fn is_vector(self) -> bool {
        self.is_array() && self.array_dimensions() == 1
    }
    pub fn is_string(self) -> bool {
        self.is_vector()
            && (self.is_simple_char_vector()
                || (self.is_adjustable_array()
                    && self.uref(ARRAY_VECTOR).is_simple_char_vector()))
    }

    /// Wrapped (fixnum) length of a vector.
    pub fn vector_length(self) -> LispObj {
        if self.is_adjustable_array() {
            self.uref(ARRAY_DIM1_LENGTH)
        } else {
            self.uref(ARRAY_SIMPLE_VECTOR_LENGTH)
        }
    }
    /// Pointer to the first data cell of a simple vector, or of an
    /// adjustable array's backing vector.
    pub fn array_start(self) -> *mut LispObj {
        let v = if self.is_adjustable_array() {
            self.uref(ARRAY_VECTOR)
        } else {
            self
        };
        unsafe { v.uvector_ptr().add(ARRAY_SIMPLE_VECTOR_START) }
    }
    pub fn char_array_start(self) -> *mut LispChar {
        self.array_start() as *mut LispChar
    }
    /// Element `i` of a simple general vector.
    pub fn svref(self, i: usize) -> LispObj {
        unsafe { *self.array_start().add(i) }
    }
    pub fn svset(self, i: usize, v: LispObj) {
        unsafe { *self.array_start().add(i) = v }
    }

    pub fn is_single_float(self) -> bool {
        self.is_uvector_of(SINGLE_FLOAT_TYPE)
    }
    pub fn is_float(self) -> bool {
        self.is_double_float() || self.is_single_float()
    }
    pub fn single_float(self) -> f32 {
        unsafe { *(self.uvector_ptr().add(SINGLE_FLOAT_OFFSET) as *const f32) }
    }
    pub fn double_float(self) -> f64 {
        unsafe { *(self.uvector_ptr().add(DOUBLE_FLOAT_OFFSET) as *const f64) }
    }

    /// Contents of a Lisp string as a Rust `String`.
    pub fn string_to_rust(self) -> String {
        debug_assert!(self.is_string());
        let len = self.vector_length().integer() as usize;
        let s = unsafe { std::slice::from_raw_parts(self.char_array_start(), len) };
        String::from_utf16_lossy(s)
    }
}

/// Uvector header word for `units` 16-byte units of type `ty`.
#[inline]
pub const fn uvector_header(units: usize, ty: usize) -> LispObj {
    LispObj((units << 8) | (ty << 3) | UVECTOR_LENGTH_TAG)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fixnum_roundtrip() {
        for n in [0i64, 1, -1, 42, -42, MOST_POSITIVE_FIXNUM, MOST_NEGATIVE_FIXNUM] {
            let o = LispObj::fixnum(n);
            assert!(o.is_fixnum());
            assert_eq!(o.integer(), n);
        }
        assert!(LispObj::fixnum_checked(MOST_POSITIVE_FIXNUM + 1).is_none());
        assert_eq!(LispObj::fixnum(5).raw(), 40);
    }

    #[test]
    fn character_tagging() {
        let c = LispObj::character('A' as u32);
        assert!(c.is_character());
        assert!(!c.is_fixnum());
        assert_eq!(c.char_code(), 65);
        assert_eq!(c.raw(), (65 << 8) | 1);
    }

    #[test]
    fn markers() {
        assert!(SYMBOL_UNBOUND.is_immediate() && !SYMBOL_UNBOUND.is_character());
        assert_eq!(uvector_header(2, ARRAY_TYPE).raw(), (2 << 8) | (3 << 3) | 6);
    }
}
