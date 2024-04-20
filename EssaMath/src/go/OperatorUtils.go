package main

// #cgo LDFLAGS: -L. -lessamath_go -lessamath
// #include "EssaMath_cxx/OperatorUtils.h"
import "C"
import "unsafe"

func abs_f(expr expression_f) expression_f {
	return expression_f{
		ptr: unsafe.Pointer(C.abs_f(expr.ptr)),
	}
}

func abs_d(expr expression_d) expression_d {
	return expression_d{
		ptr: unsafe.Pointer(C.abs_d(expr.ptr)),
	}
}

func abs_cf(expr expression_cf) expression_cf {
	return expression_cf{
		ptr: unsafe.Pointer(C.abs_cf(expr.ptr)),
	}
}

func abs_cd(expr expression_cd) expression_cd {
	return expression_cd{
		ptr: unsafe.Pointer(C.abs_cd(expr.ptr)),
	}
}

func cabs_f(expr expression_f) expression_f {
	return expression_f{
		ptr: unsafe.Pointer(C.cabs_f(expr.ptr)),
	}
}

func cabs_d(expr expression_d) expression_d {
	return expression_d{
		ptr: unsafe.Pointer(C.cabs_d(expr.ptr)),
	}
}

func cabs_cf(expr expression_cf) expression_cf {
	return expression_cf{
		ptr: unsafe.Pointer(C.cabs_cf(expr.ptr)),
	}
}

func cabs_cd(expr expression_cd) expression_cd {
	return expression_cd{
		ptr: unsafe.Pointer(C.cabs_cd(expr.ptr)),
	}
}
