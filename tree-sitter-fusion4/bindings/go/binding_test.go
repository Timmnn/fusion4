package tree_sitter_fusion4_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_fusion4 "github.com/timmnn/fusion4/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_fusion4.Language())
	if language == nil {
		t.Errorf("Error loading Fusion4 grammar")
	}
}
