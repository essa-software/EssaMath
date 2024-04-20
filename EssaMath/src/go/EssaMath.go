package main

// #cgo LDFLAGS: -L. -lessamath_go -lessamath
// #include "EssaMath_cxx/EssaMath.h"
import "C"
import "fmt"

func init_math() {
	C.init_math()
}

func free_math() {
	C.free_math()
}

func main() {
	init_math()

	_table := create_symbol_table_d()
	_table_complex := create_symbol_table_cd()
	_table.create_variable("a", 3.14)
	x := float64(1.0)
	y := complex128(1.0 + 2.0i)
	_table.add_constants()
	_table.add_variable("x", &x, false)
	_table_complex.add_constants()
	_table_complex.add_variable("y", &y, false)
	fmt.Println(_table.is_variable("x"))
	fmt.Println(_table.is_variable("y"))
	fmt.Println(_table_complex.is_variable("x"))
	fmt.Println(_table_complex.is_variable("y"))
	fmt.Println(y)

	_expression := create_expression_d()
	_expression_complex := create_expression_cd()
	_expression.register_symbol_table(&_table)
	_expression_complex.register_symbol_table(&_table_complex)

	_parser := create_parser_d()
	_parser_complex := create_parser_cd()
	_parser.compile("sin(x) * cos(x)", &_expression)
	_parser_complex.compile("5 / 2 * y + y * %i * 2", &_expression_complex)

	fmt.Println(_expression.to_string())
	fmt.Println(_expression_complex.to_string())

	for x = 0; x < 4; x = x + 0.1 {
		fmt.Println(_expression.value())
	}

	_result := cabs_cd(_expression_complex)
	fmt.Println(_result.to_string())
	fmt.Println(_expression_complex.value())

	free_math()
}
