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
const (
	NodeKeywordPane     = "pane"
	NodeKeywordBar      = "bar"
	NodeKeywordBorders  = "borders"
	NodeKeywordColorMap = "color-map"
	NodeKeywordMargins  = "margins"
	NodeKeywordSplit    = "split"
	NodeKeywordTabs     = "tabs"
)

var janetTypes = map[string]Node{
	NodeKeywordPane:     &PaneNode{},
	NodeKeywordBar:      &BarNode{},
	NodeKeywordBorders:  &BordersNode{},
	NodeKeywordColorMap: &ColorMapNode{},
	NodeKeywordMargins:  &MarginsNode{},
	NodeKeywordSplit:    &SplitNode{},
	NodeKeywordTabs:     &TabsNode{},
}
