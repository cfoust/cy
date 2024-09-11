package cmd

import (
	_ "embed"

	_ "github.com/mattn/go-sqlite3"
)

//go:embed schema.sql
var SCHEMA string
