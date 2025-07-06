package layout

const (
	NodeTypePane NodeType = iota
	NodeTypeSplit
	NodeTypeMargins
	NodeTypeBorders
	NodeTypeTabs
	NodeTypeBar
	NodeTypeColorMap
)

var janetTypes = map[string]Node{
	"tabs": &TabsNode{},
}
