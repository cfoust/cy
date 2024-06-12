package motion

//func TestJump(t *testing.T) {
//s := sessions.NewSimulator()
//size := geom.Size{R: 5, C: 50}
//s.Add(
//size,
//emu.LineFeedMode,
//"The five boxing wizards jump quickly. a",
//)

//i := createImageTest(s.Terminal(), size)
//f := createFlowTest(s.Terminal(), size)
//for _, m := range []Movement{f, i} {
//m.Jump("T", false, false)
//require.Equal(t, geom.Vec2{}, m.Cursor())
//m.MoveCursorX(1)
//m.MoveCursorX(1)
//m.Jump("T", false, false)
//require.Equal(t, geom.Vec2{}, m.Cursor())
//m.Jump("a", true, false)
//require.Equal(t, geom.Vec2{C: 19}, m.Cursor())
//m.Jump("a", true, false)
//require.Equal(t, geom.Vec2{C: 38}, m.Cursor())
//m.Jump("T", false, false)
//require.Equal(t, geom.Vec2{}, m.Cursor())
//m.Jump("x", true, true)
//require.Equal(t, geom.Vec2{C: 10}, m.Cursor())
//m.Jump("e", false, true)
//require.Equal(t, geom.Vec2{C: 8}, m.Cursor())
//}
//}
