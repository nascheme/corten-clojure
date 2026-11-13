//! Kernel tests: heap layout and immediates.

use corten::heap;
use corten::object::*;
use corten::with_lisp;

#[test]
fn uvector_layout() {
    with_lisp(|| {
        // A double: header + value = 2 cells = 1 unit.
        let d = heap::double_float_node(1.5);
        assert_eq!(d.raw() & 15, UVECTOR_TAG);
        assert_eq!(d.uvector_type(), DOUBLE_FLOAT_TYPE);
        assert_eq!(d.uvector_size(), 1);
        assert!(d.uref(0).is_uvector_header());
        let v = heap::vector_node(3);
        assert_eq!(v.uvector_size(), 3); // 1 header + 1 length + 3 = 5 -> 3 units
        assert_eq!(v.vector_length().integer(), 3);
        let str = heap::string_node("hello");
        assert!(str.is_string());
        assert_eq!(str.uvector_size(), 2); // header + length + 2 char cells
        assert_eq!(str.string_to_rust(), "hello");
    });
}

#[test]
fn nil_and_booleans() {
    assert!(NIL.is_immediate() && FALSE.is_immediate() && TRUE.is_immediate());
    assert!(!NIL.is_character() && !FALSE.is_character() && !TRUE.is_character());
    assert!(!truthy(NIL) && !truthy(FALSE) && truthy(TRUE));
    assert!(truthy(LispObj::fixnum(0)) && truthy(LispObj::character(0)));
    assert!(truthy(LispObj(NIL.raw() | (1 << 40))));
}
