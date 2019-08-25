// Clang includes
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/Expr.h"
#include "clang/AST/Decl.h"
#include "clang/AST/Type.h"
#include "clang/AST/PrettyPrinter.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Lex/PPCallbacks.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Rewrite/Core/Rewriter.h"
#include "clang/Rewrite/Frontend/FixItRewriter.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"

// LLVM includes
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"

// Standard includes
#include <memory>
#include <string>
#include <vector>
#include <type_traits>
#include <iostream>
#include <stdio.h>

namespace PointerFinder {

/// Callback class for matches on the AST.
class MatchHandlerSwitch : public clang::ast_matchers::MatchFinder::MatchCallback {
 public:
     MatchHandlerSwitch(clang::Rewriter &Rewrite) : Rewrite(Rewrite) {}
     
  using MatchResult = clang::ast_matchers::MatchFinder::MatchResult;

  /// Handles a match result for a pointer variable.
  ///
  /// Given a matched `DeclaratorDecl` (i.e. `VarDecl` or `FieldDecl`) with
  /// pointer type, verifies that if the variable is named, its name begins with
  /// a 'p_'. Otherwise emits a diagnostic and FixItHint.
  void run(const MatchResult& Result) {
    const auto* Decl = Result.Nodes.getNodeAs<clang::SwitchStmt>("switchStmt");
    const clang::SourceManager *SM = Result.SourceManager;

    llvm::outs() << "------ \nSwitch body" << "\n";
    clang::SourceRange range = Decl->getSourceRange();
    llvm::StringRef ref = clang::Lexer::getSourceText(clang::CharSourceRange::getCharRange({range.getBegin(), range.getEnd().getLocWithOffset(1)}), *SM, Result.Context->getLangOpts());
    llvm::outs() << ref.str() << "\n";

    std::string elseif;
    
    if (const clang::VarDecl *DeclCond = Decl->getConditionVariable()) {
        std::string condTxt = DeclCond->getNameAsString();
        clang::CharSourceRange condRange = clang::CharSourceRange::getTokenRange(DeclCond->getLocStart(), DeclCond->getLocEnd());
        std::string defTxt = clang::Lexer::getSourceText(condRange, *SM, Result.Context->getLangOpts()).str();
        std::cout << "Condition: " << condTxt << "\n";
        std::cout << "! Prepend this: " << defTxt << "\n";
        elseif.append(defTxt);
        elseif.append(";\n");
        elseif.append("if(");
        elseif.append(condTxt);
        elseif.append("){");
    } else {
        const auto* Cond = Decl->getCond();
        clang::CharSourceRange condRange = clang::CharSourceRange::getTokenRange(Cond->getLocStart(), Cond->getLocEnd());
        std::string condTxt = clang::Lexer::getSourceText(condRange, *SM, Result.Context->getLangOpts()).str();
        std::cout << "Condition: " << condTxt << "\n";
        elseif.append("if(");
        elseif.append(condTxt);
        elseif.append("){");
    }
    
    Rewrite.ReplaceText(range, elseif);
      
      
    /*const auto* Decl = Result.Nodes.getNodeAs<clang::SwitchStmt>("switchStmt");
    
    const auto Cond = Decl->getCond();
    
    
    clang::SourceRange range = Decl->getCond()->getSourceRange();
    const clang::SourceManager *SM = Result.SourceManager;
    
    clang::CharSourceRange conditionRange = clang::CharSourceRange::getTokenRange(Cond->getLocStart(), Cond->getLocEnd());

    llvm::StringRef condTxt = clang::Lexer::getSourceText(conditionRange, *SM, Result.Context->getLangOpts());
    
    llvm::outs() << condTxt.str() << "\n";
    return;
    
    clang::DiagnosticsEngine& Diagnostics = Result.Context->getDiagnostics();
    const unsigned ID =
        Diagnostics.getCustomDiagID(clang::DiagnosticsEngine::Warning,
                                    "asd '%0' dsa "
                                    "have a 'p_' prefix");

    clang::DiagnosticBuilder Builder = Diagnostics.Report(Decl->getSwitchLoc(), ID);
    Builder.AddString("SWITCH");
    
    const auto Start = Decl->getSwitchLoc();
    const auto End = Start.getLocWithOffset(16);
    const auto Range = clang::CharSourceRange::getCharRange({Start, End});
    Builder.AddSourceRange(Range);*/
	
  }
  private:
  clang::Rewriter &Rewrite;
};

/// Dispatches a a `MatchFinder` to look for pointer variables.
class Consumer : public clang::ASTConsumer {
 public:
     Consumer(clang::Rewriter &R) : HandlerForSwitch(R) {
         using namespace clang::ast_matchers;

        const auto Matcher2 =
            switchStmt(
                isExpansionInMainFile()
            ).bind("switchStmt");
        // clang-format on

        Matcher.addMatcher(Matcher2, &HandlerForSwitch);
     }
  /// Registers a matcher on pointers and dispatches it on the AST.
  void HandleTranslationUnit(clang::ASTContext& Context) override {
    // Run the matchers when we have the whole TU parsed.
        Matcher.matchAST(Context);
  }
  
  private:
  MatchHandlerSwitch HandlerForSwitch;
  clang::ast_matchers::MatchFinder Matcher;
};
};

/// Creates an `ASTConsumer` and logs begin and end of file processing.
class MyFrontendAction : public clang::ASTFrontendAction {
 public:
  using ASTConsumerPointer = std::unique_ptr<clang::ASTConsumer>;

  ASTConsumerPointer CreateASTConsumer(clang::CompilerInstance& Compiler,
                                       llvm::StringRef Filename) override {
    TheRewriter.setSourceMgr(Compiler.getSourceManager(), Compiler.getLangOpts());
    return std::make_unique<PointerFinder::Consumer>(TheRewriter);
  }

  bool BeginSourceFileAction(clang::CompilerInstance& Compiler,
                             llvm::StringRef Filename) override {
    //llvm::outs() << "Processing file " << Filename << '\n';
    return true;
  }

  void EndSourceFileAction() override {
    TheRewriter.getEditBuffer(TheRewriter.getSourceMgr().getMainFileID()).write(llvm::outs());
  }
  
  private:
  clang::Rewriter TheRewriter;
};
 // namespace PointerFinder

namespace {
llvm::cl::extrahelp MoreHelp("\nMakes sure pointers have a 'p_' prefix\n");
llvm::cl::OptionCategory ToolCategory("PointerFinder");
llvm::cl::extrahelp
    CommonHelp(clang::tooling::CommonOptionsParser::HelpMessage);
}  // namespace

auto main(int argc, const char* argv[]) -> int {
  using namespace clang::tooling;

  CommonOptionsParser OptionsParser(argc, argv, ToolCategory);
  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());

  auto Action = newFrontendActionFactory<MyFrontendAction>();
  return Tool.run(Action.get());
}
