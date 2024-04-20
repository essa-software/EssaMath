package main

// #cgo LDFLAGS: -L. -lessamath_go -lessamath
// #include "EssaMath_cxx/SymbolTable.h"
import "C"
import "unsafe"

type symbol_table_f struct {
	ptr unsafe.Pointer
}

type symbol_table_d struct {
	ptr unsafe.Pointer
}

type symbol_table_cf struct {
	ptr unsafe.Pointer
}

type symbol_table_cd struct {
	ptr unsafe.Pointer
}

func create_symbol_table_f() symbol_table_f {
	return symbol_table_f{
		ptr: C.create_symbol_table_f(),
	}
}

func create_symbol_table_d() symbol_table_d {
	return symbol_table_d{
		ptr: C.create_symbol_table_d(),
	}
}

func create_symbol_table_cf() symbol_table_cf {
	return symbol_table_cf{
		ptr: C.create_symbol_table_cf(),
	}
}

func create_symbol_table_cd() symbol_table_cd {
	return symbol_table_cd{
		ptr: C.create_symbol_table_cd(),
	}
}

func clone_symbol_table_f(_other symbol_table_f) symbol_table_f {
	return symbol_table_f{
		ptr: C.clone_symbol_table_f(_other.ptr),
	}
}

func clone_symbol_table_d(_other symbol_table_d) symbol_table_d {
	return symbol_table_d{
		ptr: C.clone_symbol_table_d(_other.ptr),
	}
}

func clone_symbol_table_cf(_other symbol_table_cf) symbol_table_cf {
	return symbol_table_cf{
		ptr: C.clone_symbol_table_cf(_other.ptr),
	}
}

func clone_symbol_table_cd(_other symbol_table_cd) symbol_table_cd {
	return symbol_table_cd{
		ptr: C.clone_symbol_table_cd(_other.ptr),
	}
}

func (_self symbol_table_f) Destroy() {
	C.destroy_symbol_table_f(_self.ptr)
}

func (_self symbol_table_d) Destroy() {
	C.destroy_symbol_table_d(_self.ptr)
}

func (_self symbol_table_cf) Destroy() {
	C.destroy_symbol_table_cf(_self.ptr)
}

func (_self symbol_table_cd) Destroy() {
	C.destroy_symbol_table_cd(_self.ptr)
}

func (_self symbol_table_f) Assign(_other symbol_table_f) {
	C.destroy_symbol_table_f(_self.ptr)
}

func (_self symbol_table_d) Assign(_other symbol_table_d) {
	C.assign_symbol_table_d(_self.ptr, _other.ptr)
}

func (_self symbol_table_cf) Assign(_other symbol_table_cf) {
	C.assign_symbol_table_cf(_self.ptr, _other.ptr)
}

func (_self symbol_table_cd) Assign(_other symbol_table_cd) {
	C.assign_symbol_table_cd(_self.ptr, _other.ptr)
}

func (_self symbol_table_f) Compare(_other symbol_table_f) bool {
	return C.compare_symbol_table_f(_self.ptr, _other.ptr) == 1
}

func (_self symbol_table_d) Compare(_other symbol_table_d) bool {
	return C.compare_symbol_table_d(_self.ptr, _other.ptr) == 1
}

func (_self symbol_table_cf) Compare(_other symbol_table_cf) bool {
	return C.compare_symbol_table_cf(_self.ptr, _other.ptr) == 1
}

func (_self symbol_table_cd) Compare(_other symbol_table_cd) bool {
	return C.compare_symbol_table_cd(_self.ptr, _other.ptr) == 1
}

func (_self symbol_table_f) mutability() int {
	return int(C.mutability_symbol_table_f(_self.ptr))
}

func (_self symbol_table_d) mutability() int {
	return int(C.mutability_symbol_table_d(_self.ptr))
}

func (_self symbol_table_cf) mutability() int {
	return int(C.mutability_symbol_table_cf(_self.ptr))
}

func (_self symbol_table_cd) mutability() int {
	return int(C.mutability_symbol_table_cd(_self.ptr))
}

func B2i(b bool) C.int {
	if b {
		return 1
	}
	return 0
}

func (_self symbol_table_f) clear_variables(delete_node bool) {
	C.clear_variables_symbol_table_f(_self.ptr, B2i(delete_node))
}

func (_self symbol_table_d) clear_variables(delete_node bool) {
	C.clear_variables_symbol_table_d(_self.ptr, B2i(delete_node))
}

func (_self symbol_table_cf) clear_variables(delete_node bool) {
	C.clear_variables_symbol_table_cf(_self.ptr, B2i(delete_node))
}

func (_self symbol_table_cd) clear_variables(delete_node bool) {
	C.clear_variables_symbol_table_cd(_self.ptr, B2i(delete_node))
}

func (_self symbol_table_f) clear_vectors() {
	C.clear_vectors_symbol_table_f(_self.ptr)
}

func (_self symbol_table_d) clear_vectors() {
	C.clear_vectors_symbol_table_d(_self.ptr)
}

func (_self symbol_table_cf) clear_vectors() {
	C.clear_vectors_symbol_table_cf(_self.ptr)
}

func (_self symbol_table_cd) clear_vectors() {
	C.clear_vectors_symbol_table_cd(_self.ptr)
}

func (_self symbol_table_f) clear_local_constants() {
	C.clear_local_constants_symbol_table_f(_self.ptr)
}

func (_self symbol_table_d) clear_local_constants() {
	C.clear_local_constants_symbol_table_d(_self.ptr)
}

func (_self symbol_table_cf) clear_local_constants() {
	C.clear_local_constants_symbol_table_cf(_self.ptr)
}

func (_self symbol_table_cd) clear_local_constants() {
	C.clear_local_constants_symbol_table_cd(_self.ptr)
}

func (_self symbol_table_f) clear() {
	C.clear_symbol_table_f(_self.ptr)
}

func (_self symbol_table_d) clear() {
	C.clear_symbol_table_d(_self.ptr)
}

func (_self symbol_table_cf) clear() {
	C.clear_symbol_table_cf(_self.ptr)
}

func (_self symbol_table_cd) clear() {
	C.clear_symbol_table_cd(_self.ptr)
}
func (_self symbol_table_f) variable_count() int {
	return int(C.variable_count_symbol_table_f(_self.ptr))
}

func (_self symbol_table_d) variable_count() int {
	return int(C.variable_count_symbol_table_d(_self.ptr))
}

func (_self symbol_table_cf) variable_count() int {
	return int(C.variable_count_symbol_table_cf(_self.ptr))
}

func (_self symbol_table_cd) variable_count() int {
	return int(C.variable_count_symbol_table_cd(_self.ptr))
}

func (_self symbol_table_f) vector_count() int {
	return int(C.vector_count_symbol_table_f(_self.ptr))
}

func (_self symbol_table_d) vector_count() int {
	return int(C.vector_count_symbol_table_d(_self.ptr))
}

func (_self symbol_table_cf) vector_count() int {
	return int(C.vector_count_symbol_table_cf(_self.ptr))
}

func (_self symbol_table_cd) vector_count() int {
	return int(C.vector_count_symbol_table_cd(_self.ptr))
}

func (_self symbol_table_f) is_constant_node(symbol_name string) bool {
	sbytes := []byte(symbol_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.is_constant_node_symbol_table_f(_self.ptr, ccp) == 1
}

func (_self symbol_table_d) is_constant_node(symbol_name string) bool {
	sbytes := []byte(symbol_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.is_constant_node_symbol_table_d(_self.ptr, ccp) == 1
}

func (_self symbol_table_cf) is_constant_node(symbol_name string) bool {
	sbytes := []byte(symbol_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.is_constant_node_symbol_table_cf(_self.ptr, ccp) == 1
}

func (_self symbol_table_cd) is_constant_node(symbol_name string) bool {
	sbytes := []byte(symbol_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.is_constant_node_symbol_table_cd(_self.ptr, ccp) == 1
}

func (_self symbol_table_f) create_variable(variable_name string, value float32) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.create_variable_symbol_table_f(_self.ptr, ccp, unsafe.Pointer(&value)) == 1
}

func (_self symbol_table_d) create_variable(variable_name string, value float64) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.create_variable_symbol_table_d(_self.ptr, ccp, unsafe.Pointer(&value)) == 1
}

func (_self symbol_table_cf) create_variable(variable_name string, value complex64) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.create_variable_symbol_table_cf(_self.ptr, ccp, unsafe.Pointer(&value)) == 1
}

func (_self symbol_table_cd) create_variable(variable_name string, value complex128) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.create_variable_symbol_table_cd(_self.ptr, ccp, unsafe.Pointer(&value)) == 1
}

func (_self symbol_table_f) add_variable(variable_name string, value *float32, is_constant bool) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.add_variable_symbol_table_f(_self.ptr, ccp, unsafe.Pointer(value), B2i(is_constant)) == 1
}

func (_self symbol_table_d) add_variable(variable_name string, value *float64, is_constant bool) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.add_variable_symbol_table_d(_self.ptr, ccp, unsafe.Pointer(value), B2i(is_constant)) == 1
}

func (_self symbol_table_cf) add_variable(variable_name string, value *complex64, is_constant bool) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.add_variable_symbol_table_cf(_self.ptr, ccp, unsafe.Pointer(value), B2i(is_constant)) == 1
}

func (_self symbol_table_cd) add_variable(variable_name string, value *complex128, is_constant bool) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.add_variable_symbol_table_cd(_self.ptr, ccp, unsafe.Pointer(value), B2i(is_constant)) == 1
}

func (_self symbol_table_f) add_constant(variable_name string, value float32) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.add_constant_symbol_table_f(_self.ptr, ccp, unsafe.Pointer(&value)) == 1
}

func (_self symbol_table_d) add_constant(variable_name string, value float64) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.add_constant_symbol_table_d(_self.ptr, ccp, unsafe.Pointer(&value)) == 1
}

func (_self symbol_table_cf) add_constant(variable_name string, value complex64) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.add_constant_symbol_table_cf(_self.ptr, ccp, unsafe.Pointer(&value)) == 1
}

func (_self symbol_table_cd) add_constant(variable_name string, value complex128) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.add_constant_symbol_table_cd(_self.ptr, ccp, unsafe.Pointer(&value)) == 1
}

func (_self symbol_table_f) remove_variable(variable_name string, delete_node bool) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.remove_variable_symbol_table_f(_self.ptr, ccp, B2i(delete_node)) == 1
}

func (_self symbol_table_d) remove_variable(variable_name string, delete_node bool) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.remove_variable_symbol_table_d(_self.ptr, ccp, B2i(delete_node)) == 1
}

func (_self symbol_table_cf) remove_variable(variable_name string, delete_node bool) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.remove_variable_symbol_table_cf(_self.ptr, ccp, B2i(delete_node)) == 1
}

func (_self symbol_table_cd) remove_variable(variable_name string, delete_node bool) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.remove_variable_symbol_table_cd(_self.ptr, ccp, B2i(delete_node)) == 1
}

func (_self symbol_table_f) remove_vector(variable_name string) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.remove_vector_symbol_table_f(_self.ptr, ccp) == 1
}

func (_self symbol_table_d) remove_vector(variable_name string) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.remove_vector_symbol_table_d(_self.ptr, ccp) == 1
}

func (_self symbol_table_cf) remove_vector(variable_name string) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.remove_vector_symbol_table_cf(_self.ptr, ccp) == 1
}

func (_self symbol_table_cd) remove_vector(variable_name string) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.remove_vector_symbol_table_cd(_self.ptr, ccp) == 1
}

func (_self symbol_table_f) add_constants() bool {
	return C.add_constants_symbol_table_f(_self.ptr) == 1
}

func (_self symbol_table_d) add_constants() bool {
	return C.add_constants_symbol_table_d(_self.ptr) == 1
}

func (_self symbol_table_cf) add_constants() bool {
	return C.add_constants_symbol_table_cf(_self.ptr) == 1
}

func (_self symbol_table_cd) add_constants() bool {
	return C.add_constants_symbol_table_cd(_self.ptr) == 1
}

func (_self symbol_table_f) add_pi() bool {
	return C.add_pi_symbol_table_f(_self.ptr) == 1
}

func (_self symbol_table_d) add_pi() bool {
	return C.add_pi_symbol_table_d(_self.ptr) == 1
}

func (_self symbol_table_cf) add_pi() bool {
	return C.add_pi_symbol_table_cf(_self.ptr) == 1
}

func (_self symbol_table_cd) add_pi() bool {
	return C.add_pi_symbol_table_cd(_self.ptr) == 1
}

func (_self symbol_table_f) add_e() bool {
	return C.add_e_symbol_table_f(_self.ptr) == 1
}

func (_self symbol_table_d) add_e() bool {
	return C.add_e_symbol_table_d(_self.ptr) == 1
}

func (_self symbol_table_cf) add_e() bool {
	return C.add_e_symbol_table_cf(_self.ptr) == 1
}

func (_self symbol_table_cd) add_e() bool {
	return C.add_e_symbol_table_cd(_self.ptr) == 1
}

func (_self symbol_table_f) add_i() bool {
	return C.add_i_symbol_table_f(_self.ptr) == 1
}

func (_self symbol_table_d) add_i() bool {
	return C.add_i_symbol_table_d(_self.ptr) == 1
}

func (_self symbol_table_cf) add_i() bool {
	return C.add_i_symbol_table_cf(_self.ptr) == 1
}

func (_self symbol_table_cd) add_i() bool {
	return C.add_i_symbol_table_cd(_self.ptr) == 1
}

func (_self symbol_table_f) add_epsilon() bool {
	return C.add_epsilon_symbol_table_f(_self.ptr) == 1
}

func (_self symbol_table_d) add_epsilon() bool {
	return C.add_epsilon_symbol_table_d(_self.ptr) == 1
}

func (_self symbol_table_cf) add_epsilon() bool {
	return C.add_epsilon_symbol_table_cf(_self.ptr) == 1
}

func (_self symbol_table_cd) add_epsilon() bool {
	return C.add_epsilon_symbol_table_cd(_self.ptr) == 1
}

func (_self symbol_table_f) add_infinity() bool {
	return C.add_infinity_symbol_table_f(_self.ptr) == 1
}

func (_self symbol_table_d) add_infinity() bool {
	return C.add_infinity_symbol_table_d(_self.ptr) == 1
}

func (_self symbol_table_cf) add_infinity() bool {
	return C.add_infinity_symbol_table_cf(_self.ptr) == 1
}

func (_self symbol_table_cd) add_infinity() bool {
	return C.add_infinity_symbol_table_cd(_self.ptr) == 1
}

func (_self symbol_table_f) symbol_exists(variable_name string, check_reserved_symb bool) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.symbol_exists_symbol_table_f(_self.ptr, ccp, B2i(check_reserved_symb)) == 1
}

func (_self symbol_table_d) symbol_exists(variable_name string, check_reserved_symb bool) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.symbol_exists_symbol_table_d(_self.ptr, ccp, B2i(check_reserved_symb)) == 1
}

func (_self symbol_table_cf) symbol_exists(variable_name string, check_reserved_symb bool) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.symbol_exists_symbol_table_cf(_self.ptr, ccp, B2i(check_reserved_symb)) == 1
}

func (_self symbol_table_cd) symbol_exists(variable_name string, check_reserved_symb bool) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.symbol_exists_symbol_table_cd(_self.ptr, ccp, B2i(check_reserved_symb)) == 1
}

func (_self symbol_table_f) is_variable(variable_name string) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.is_variable_symbol_table_f(_self.ptr, ccp) == 1
}

func (_self symbol_table_d) is_variable(variable_name string) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.is_variable_symbol_table_d(_self.ptr, ccp) == 1
}

func (_self symbol_table_cf) is_variable(variable_name string) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.is_variable_symbol_table_cf(_self.ptr, ccp) == 1
}

func (_self symbol_table_cd) is_variable(variable_name string) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.is_variable_symbol_table_cd(_self.ptr, ccp) == 1
}

func (_self symbol_table_f) is_vector(variable_name string) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.is_vector_symbol_table_f(_self.ptr, ccp) == 1
}

func (_self symbol_table_d) is_vector(variable_name string) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.is_vector_symbol_table_d(_self.ptr, ccp) == 1
}

func (_self symbol_table_cf) is_vector(variable_name string) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.is_vector_symbol_table_cf(_self.ptr, ccp) == 1
}

func (_self symbol_table_cd) is_vector(variable_name string) bool {
	sbytes := []byte(variable_name)
	ccp := (*C.char)(unsafe.Pointer(&sbytes[0]))
	return C.is_vector_symbol_table_cd(_self.ptr, ccp) == 1
}

func (_self symbol_table_f) valid() bool {
	return C.valid_symbol_table_f(_self.ptr) == 1
}

func (_self symbol_table_d) valid() bool {
	return C.valid_symbol_table_d(_self.ptr) == 1
}

func (_self symbol_table_cf) valid() bool {
	return C.valid_symbol_table_cf(_self.ptr) == 1
}

func (_self symbol_table_cd) valid() bool {
	return C.valid_symbol_table_cd(_self.ptr) == 1
}
