package janet

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func cmp[T any](t *testing.T, before T) {
	value, err := marshal(before)
	assert.NoError(t, err)

	var after T
	err = unmarshal(value, &after)
	assert.NoError(t, err)

	t.Logf("%+v", after)
	assert.Equal(t, before, after, "should yield same result")
}

func TestTranslation(t *testing.T) {
	// Starting a VM in this test is not enough; we're creating values on
	// the heap and need to guarantee that Janet was initialized
	initJanet()
	defer deInitJanet()

	cmp(t, 2)
	cmp(t, 2.02)
	cmp(t, true)
	cmp(t, "test")

	type Value struct {
		One   int
		Two   bool
		Three string
		Ints  [6]int
		Bools []bool
	}

	bools := make([]bool, 2)
	bools[0] = true
	cmp(t, Value{
		One:   2,
		Two:   true,
		Three: "test",
		Ints: [6]int{
			2,
			3,
		},
		Bools: bools,
	})
}

//func TestMain(m *testing.M) {
//ctx, cancel := context.WithCancel(context.Background())
//defer cancel()
//vm, _ := New(ctx)
//vm.Execute("")
//m.Run()
//}
