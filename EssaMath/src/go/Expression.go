package main

// #cgo LDFLAGS: -L. -lessamath_go -lessamath
// #include "EssaMath_cxx/Expression.h"
import "C"
import (
	"unsafe"
)

type expression_f struct {
	ptr unsafe.Pointer
}

type expression_d struct {
	ptr unsafe.Pointer
}

type expression_cf struct {
	ptr unsafe.Pointer
}

type expression_cd struct {
	ptr unsafe.Pointer
}

func create_expression_f() expression_f {
	return expression_f{
		ptr: C.create_expression_f(),
	}
}

func create_expression_d() expression_d {
	return expression_d{
		ptr: C.create_expression_d(),
	}
}

func create_expression_cf() expression_cf {
	return expression_cf{
		ptr: C.create_expression_cf(),
	}
}

func create_expression_cd() expression_cd {
	return expression_cd{
		ptr: C.create_expression_cd(),
	}
}

func clone_expression_f(_other expression_f) expression_f {
	return expression_f{
		ptr: C.clone_expression_f(_other.ptr),
	}
}

func clone_expression_d(_other expression_d) expression_d {
	return expression_d{
		ptr: C.clone_expression_d(_other.ptr),
	}
}

func clone_expression_cf(_other expression_cf) expression_cf {
	return expression_cf{
		ptr: C.clone_expression_cf(_other.ptr),
	}
}

func clone_expression_cd(_other expression_cd) expression_cd {
	return expression_cd{
		ptr: C.clone_expression_cd(_other.ptr),
	}
}

func (_self expression_f) Destroy() {
	C.destroy_expression_f(_self.ptr)
}

func (_self expression_d) Destroy() {
	C.destroy_expression_d(_self.ptr)
}

func (_self expression_cf) Destroy() {
	C.destroy_expression_cf(_self.ptr)
}

func (_self expression_cd) Destroy() {
	C.destroy_expression_cd(_self.ptr)
}

func (_self expression_f) Assign(_other expression_f) {
	C.assign_expression_f(_self.ptr, _other.ptr)
}

func (_self expression_d) Assign(_other expression_d) {
	C.assign_expression_d(_self.ptr, _other.ptr)
}

func (_self expression_cf) Assign(_other expression_cf) {
	C.assign_expression_cf(_self.ptr, _other.ptr)
}

func (_self expression_cd) Assign(_other expression_cd) {
	C.assign_expression_cd(_self.ptr, _other.ptr)
}

func (_self expression_f) Compare(_other expression_f) bool {
	return C.compare_expression_f(_self.ptr, _other.ptr) == 1
}

func (_self expression_d) Compare(_other expression_d) bool {
	return C.compare_expression_d(_self.ptr, _other.ptr) == 1
}

func (_self expression_cf) Compare(_other expression_cf) bool {
	return C.compare_expression_cf(_self.ptr, _other.ptr) == 1
}

func (_self expression_cd) Compare(_other expression_cd) bool {
	return C.compare_expression_cd(_self.ptr, _other.ptr) == 1
}

func (_self expression_f) Negate() bool {
	return C.negate_expression_f(_self.ptr) == 1
}

func (_self expression_d) Negate() bool {
	return C.negate_expression_d(_self.ptr) == 1
}

func (_self expression_cf) Negate() bool {
	return C.negate_expression_cf(_self.ptr) == 1
}

func (_self expression_cd) Negate() bool {
	return C.negate_expression_cd(_self.ptr) == 1
}

func (_self expression_f) value() float32 {
	_valptr := C.value_expression_f(_self.ptr)
	_data := (*(*[1<<31 - 1]byte)(_valptr))[:unsafe.Sizeof(float32(0.0))]
	_val := (*(*float32)(unsafe.Pointer(&_data[0])))

	C.free_value_expression_f(_valptr)
	return _val
}

func (_self expression_d) value() float64 {
	_valptr := C.value_expression_d(_self.ptr)
	_data := (*(*[1<<31 - 1]byte)(_valptr))[:unsafe.Sizeof(float64(0.0))]
	_val := (*(*float64)(unsafe.Pointer(&_data[0])))

	C.free_value_expression_d(_valptr)
	return _val
}

func (_self expression_cf) value() complex64 {
	_valptr := C.value_expression_cf(_self.ptr)
	_data := (*(*[1<<31 - 1]byte)(_valptr))[:unsafe.Sizeof(complex64(0.0))]
	_val := (*(*complex64)(unsafe.Pointer(&_data[0])))

	C.free_value_expression_cf(_valptr)
	return _val
}

func (_self expression_cd) value() complex128 {
	_valptr := C.value_expression_cd(_self.ptr)
	_data := (*(*[1<<31 - 1]byte)(_valptr))[:unsafe.Sizeof(complex128(0.0))]
	_val := (*(*complex128)(unsafe.Pointer(&_data[0])))

	C.free_value_expression_cd(_valptr)
	return _val
}

func (_self expression_f) is_true() bool {
	return C.is_true_expression_f(_self.ptr) == 1
}

func (_self expression_d) is_true() bool {
	return C.is_true_expression_d(_self.ptr) == 1
}

func (_self expression_cf) is_true() bool {
	return C.is_true_expression_cf(_self.ptr) == 1
}

func (_self expression_cd) is_true() bool {
	return C.is_true_expression_cd(_self.ptr) == 1
}

func (_self expression_f) register_symbol_table(_table *symbol_table_f) {
	C.register_symbol_table_expression_f(_self.ptr, _table.ptr)
}

func (_self expression_d) register_symbol_table(_table *symbol_table_d) {
	C.register_symbol_table_expression_d(_self.ptr, _table.ptr)
}

func (_self expression_cf) register_symbol_table(_table *symbol_table_cf) {
	C.register_symbol_table_expression_cf(_self.ptr, _table.ptr)
}

func (_self expression_cd) register_symbol_table(_table *symbol_table_cd) {
	C.register_symbol_table_expression_cd(_self.ptr, _table.ptr)
}

func (_self expression_f) get_symbol_table(index int) symbol_table_f {
	return symbol_table_f{
		ptr: C.get_symbol_table_expression_f(_self.ptr, C.int(index)),
	}
}

func (_self expression_d) get_symbol_table(index int) symbol_table_d {
	return symbol_table_d{
		ptr: C.get_symbol_table_expression_d(_self.ptr, C.int(index)),
	}
}

func (_self expression_cf) get_symbol_table(index int) symbol_table_cf {
	return symbol_table_cf{
		ptr: C.get_symbol_table_expression_cf(_self.ptr, C.int(index)),
	}
}

func (_self expression_cd) get_symbol_table(index int) symbol_table_cd {
	return symbol_table_cd{
		ptr: C.get_symbol_table_expression_cd(_self.ptr, C.int(index)),
	}
}

func (_self expression_f) to_string() string {
	var _ptr *C.char = C.to_string_expression_f(_self.ptr)
	_result := C.GoString(_ptr)

	return _result
}

func (_self expression_d) to_string() string {
	var _ptr *C.char = C.to_string_expression_d(_self.ptr)
	_result := C.GoString(_ptr)

	return _result
}

func (_self expression_cf) to_string() string {
	var _ptr *C.char = C.to_string_expression_cf(_self.ptr)
	_result := C.GoString(_ptr)

	return _result
}

func (_self expression_cd) to_string() string {
	var _ptr *C.char = C.to_string_expression_cd(_self.ptr)
	_result := C.GoString(_ptr)

	return _result
}
