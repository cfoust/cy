package test

type Hidden struct {
	A, B int
	// invalid hidden fields should not cause issues
	c    complex128
}
