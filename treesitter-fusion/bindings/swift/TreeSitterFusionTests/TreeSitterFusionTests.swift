import XCTest
import SwiftTreeSitter
import TreeSitterFusion

final class TreeSitterFusionTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_fusion())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Fusion grammar")
    }
}
