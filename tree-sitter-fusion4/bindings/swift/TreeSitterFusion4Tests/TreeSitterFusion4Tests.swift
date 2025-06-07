import XCTest
import SwiftTreeSitter
import TreeSitterFusion4

final class TreeSitterFusion4Tests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_fusion4())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Fusion4 grammar")
    }
}
