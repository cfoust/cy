package layout

const (
	NodeTypeView NodeType = iota
	NodeTypeSplit
	NodeTypeMargins
	NodeTypeBorders
	NodeTypeTabs
	NodeTypeBar
	NodeTypeColorMap
	NodeTypeStack
)
const (
	NodeKeywordView     = "view"
	NodeKeywordBar      = "bar"
	NodeKeywordBorders  = "borders"
	NodeKeywordColorMap = "color-map"
	NodeKeywordMargins  = "margins"
	NodeKeywordSplit    = "split"
	NodeKeywordTabs     = "tabs"
	NodeKeywordStack    = "stack"
)

var janetTypes = map[string]Node{
	NodeKeywordView:     &ViewNode{},
	NodeKeywordBar:      &BarNode{},
	NodeKeywordBorders:  &BordersNode{},
	NodeKeywordColorMap: &ColorMapNode{},
	NodeKeywordMargins:  &MarginsNode{},
	NodeKeywordSplit:    &SplitNode{},
	NodeKeywordTabs:     &TabsNode{},
	NodeKeywordStack:    &StackNode{},
}
