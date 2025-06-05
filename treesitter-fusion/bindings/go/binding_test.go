package tree_sitter_fusion_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_fusion "github.com/tree-sitter/tree-sitter-fusion/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_fusion.Language())
	if language == nil {
		t.Errorf("Error loading Fusion grammar")
	}
}
