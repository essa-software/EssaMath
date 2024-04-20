package main

// #cgo LDFLAGS: -L. -lessamath_go -lessamath
// #include "EssaMath_cxx/Parser.h"
import "C"
import (
	"unsafe"
)

type parser_f struct {
	ptr unsafe.Pointer
}

type parser_d struct {
	ptr unsafe.Pointer
}

type parser_cf struct {
	ptr unsafe.Pointer
}

type parser_cd struct {
	ptr unsafe.Pointer
}

func create_parser_f() parser_f {
	return parser_f{
		ptr: C.create_parser_f(),
	}
}

func create_parser_d() parser_d {
	return parser_d{
		ptr: C.create_parser_d(),
	}
}

func create_parser_cf() parser_cf {
	return parser_cf{
		ptr: C.create_parser_cf(),
	}
}

func create_parser_cd() parser_cd {
	return parser_cd{
		ptr: C.create_parser_cd(),
	}
}

func (_self parser_f) Destroy() {
	C.destroy_parser_f(_self.ptr)
}

func (_self parser_d) Destroy() {
	C.destroy_parser_d(_self.ptr)
}

func (_self parser_cf) Destroy() {
	C.destroy_parser_cf(_self.ptr)
}

func (_self parser_cd) Destroy() {
	C.destroy_parser_cd(_self.ptr)
}

func (_self parser_f) compile(expression_string string, expr *expression_f) bool {
	sbytes := []byte(expression_string)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.compile_parser_f(_self.ptr, ccp, expr.ptr) == 1
}

func (_self parser_d) compile(expression_string string, expr *expression_d) bool {
	sbytes := []byte(expression_string)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.compile_parser_d(_self.ptr, ccp, expr.ptr) == 1
}

func (_self parser_cf) compile(expression_string string, expr *expression_cf) bool {
	sbytes := []byte(expression_string)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.compile_parser_cf(_self.ptr, ccp, expr.ptr) == 1
}

func (_self parser_cd) compile(expression_string string, expr *expression_cd) bool {
	sbytes := []byte(expression_string)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.compile_parser_cd(_self.ptr, ccp, expr.ptr) == 1
}

func (_self parser_f) compile_and_create_expr(expression_string string, symtab *symbol_table_f) expression_f {
	sbytes := []byte(expression_string)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return expression_f{
		ptr: C.compile_and_create_expression_parser_f(_self.ptr, ccp, symtab.ptr),
	}
}

func (_self parser_d) compile_and_create_expr(expression_string string, symtab *symbol_table_d) expression_d {
	sbytes := []byte(expression_string)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return expression_d{
		ptr: C.compile_and_create_expression_parser_d(_self.ptr, ccp, symtab.ptr),
	}
}

func (_self parser_cf) compile_and_create_expr(expression_string string, symtab *symbol_table_cf) expression_cf {
	sbytes := []byte(expression_string)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return expression_cf{
		ptr: C.compile_and_create_expression_parser_cf(_self.ptr, ccp, symtab.ptr),
	}
}

func (_self parser_cd) compile_and_create_expr(expression_string string, symtab *symbol_table_cd) expression_cd {
	sbytes := []byte(expression_string)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return expression_cd{
		ptr: C.compile_and_create_expression_parser_cd(_self.ptr, ccp, symtab.ptr),
	}
}

func (_self parser_f) error() string {
	var _ptr *C.char = C.error_parser_f(_self.ptr)
	_result := C.GoString(_ptr)

	return _result
}

func (_self parser_d) error() string {
	var _ptr *C.char = C.error_parser_d(_self.ptr)
	_result := C.GoString(_ptr)

	return _result
}

func (_self parser_cf) error() string {
	var _ptr *C.char = C.error_parser_cf(_self.ptr)
	_result := C.GoString(_ptr)

	return _result
}

func (_self parser_cd) error() string {
	var _ptr *C.char = C.error_parser_cd(_self.ptr)
	_result := C.GoString(_ptr)

	return _result
}

func (_self parser_f) error_count() int {
	return int(C.error_count_parser_f(_self.ptr))
}

func (_self parser_d) error_count() int {
	return int(C.error_count_parser_d(_self.ptr))
}

func (_self parser_cf) error_count() int {
	return int(C.error_count_parser_cf(_self.ptr))
}

func (_self parser_cd) error_count() int {
	return int(C.error_count_parser_cd(_self.ptr))
}
