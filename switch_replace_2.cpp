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

using namespace clang;

bool hasBreak(std::string &str){
    auto n = str.find("break;");
    return !(n == std::string::npos);
}

void eraseBreak(std::string &str){
    auto n = str.find("break;");
    if(n != std::string::npos)
        str.erase(n, 6);
}

namespace PointerFinder {

/// Callback class for matches on the AST.
    class MatchHandlerSwitch : public clang::ast_matchers::MatchFinder::MatchCallback {
    public:
        std::string extractStringFromExpr(
                const clang::Stmt *expr,
                const clang::SourceManager &SM,
                const clang::LangOptions &LangOpts
        ) {
            clang::CharSourceRange scRange = clang::CharSourceRange::getTokenRange(expr->getLocStart(), expr->getLocEnd());
            return clang::Lexer::getSourceText(scRange, SM, LangOpts).str();
        }

        MatchHandlerSwitch(clang::Rewriter &Rewrite) : Rewrite(Rewrite) {}

        using MatchResult = clang::ast_matchers::MatchFinder::MatchResult;

        /// Handles a match result for a pointer variable.
        ///
        /// Given a matched `DeclaratorDecl` (i.e. `VarDecl` or `FieldDecl`) with
        /// pointer type, verifies that if the variable is named, its name begins with
        /// a 'p_'. Otherwise emits a diagnostic and FixItHint.
        void run(const MatchResult &Result) {

            const SwitchStmt *switchStatement = Result.Nodes.getNodeAs<clang::SwitchStmt>("switchStmt");
            const clang::SourceManager *SM = Result.SourceManager;
            SourceRange sourceRange;

            //making a string from the switch condition
            const Expr *condExpr = switchStatement->getCond();
            bool invalid;
            std::string condition = Lexer::getSourceText(CharSourceRange(condExpr->getSourceRange(), true),
                                                         *SM, Result.Context->getLangOpts(), &invalid).str();
            bool isVariable = find_if(condition.begin(), condition.end(),
                                      [](char c) { return !(isalnum(c)); }) == condition.end();
            if(!isVariable){
                condition.insert(0, 1, '(').append(")");
            }
            condition.append(" == ");

            const SwitchCase *caseList = switchStatement->getSwitchCaseList();
            //checking if the switch has a default case
            bool hasDefault = false;
            sourceRange.setBegin(caseList->getKeywordLoc());
            sourceRange.setEnd(caseList->getKeywordLoc().getLocWithOffset(6));
            std::string lastCase = Lexer::getSourceText(CharSourceRange(sourceRange, true),
                                                        *SM, Result.Context->getLangOpts(), &invalid).str();
            if(lastCase.compare("default") == 0){
                hasDefault = true;
            }

            //creating a vector of statement strings
            //Note: starts from bottom to top case
            const Stmt *body = switchStatement->getBody();
            sourceRange.setBegin(caseList->getColonLoc().getLocWithOffset(1));
            sourceRange.setEnd(body->getLocEnd().getLocWithOffset(-1));
            std::vector<std::string> caseStatements;

            caseStatements.push_back(Lexer::getSourceText(CharSourceRange(sourceRange, true),
                                                          *SM, Result.Context->getLangOpts(), &invalid).str());
            while(caseList->getNextSwitchCase() != nullptr){
                sourceRange.setBegin(caseList->getNextSwitchCase()->getColonLoc().getLocWithOffset(1));
                sourceRange.setEnd(caseList->getKeywordLoc().getLocWithOffset(-1));
                caseStatements.emplace_back(Lexer::getSourceText(CharSourceRange(sourceRange, true),
                                                                 *SM, Result.Context->getLangOpts(), &invalid).str());
                caseList = caseList->getNextSwitchCase();
            }

            //Note: for fall throughs we need to move from right to left
            //to respect the case order
            int i;
            for(i = caseStatements.size()-1; i>=0; i--){
                if(!hasBreak(caseStatements[i])){
                    for(int j=i-1; j>=0; j--){
                        if(hasBreak(caseStatements[j])){
                            caseStatements[i].append(caseStatements[j]);
                            eraseBreak(caseStatements[i]);
                            break;
                        }
                        caseStatements[i].append(caseStatements[j]);
                    }
                }
                else
                    eraseBreak(caseStatements[i]);
            }

            //adding if-elseif-else
            //reseting back after making the vector
            caseList = switchStatement->getSwitchCaseList();
            //Note: we are moving through the vector from
            //left to right when falling through
            bool defaultCaseRewritten = false;
            int numberOfStmts=caseStatements.size();
            i=0;
            SourceLocation nextCaseLoc = body->getLocEnd();

            while(caseList != nullptr && i<numberOfStmts){
                if(hasDefault == true && defaultCaseRewritten == false){
                    Rewrite.InsertTextBefore(caseList->getKeywordLoc(), "}else{\n");
                    Rewrite.ReplaceText(caseList->getKeywordLoc(), 7, "");
                    Rewrite.ReplaceText(caseList->getColonLoc(), 1, "");

                    sourceRange.setBegin(caseList->getColonLoc().getLocWithOffset(1));
                    sourceRange.setEnd(nextCaseLoc.getLocWithOffset(-1));
                    //indentCaseStmt(caseStatements[i]);
                    Rewrite.ReplaceText(sourceRange, caseStatements[i]);

                    defaultCaseRewritten = true;
                    nextCaseLoc = caseList->getKeywordLoc();
                    caseList = caseList->getNextSwitchCase();
                    i++;
                    continue;
                }
                if(caseList->getNextSwitchCase() == nullptr){
                    Rewrite.InsertTextBefore(caseList->getKeywordLoc(), std::string{"if("}.append(condition));
                }else{
                    Rewrite.InsertTextBefore(caseList->getKeywordLoc(), std::string{"}else if("}.append(condition));
                }
                Rewrite.ReplaceText(caseList->getKeywordLoc(), 4, "");
                Rewrite.ReplaceText(caseList->getColonLoc(), 1, "){\n");

                //indentCaseStmt(caseStatements[i]);
                sourceRange.setBegin(caseList->getColonLoc().getLocWithOffset(1));
                sourceRange.setEnd(nextCaseLoc.getLocWithOffset(-1));
                Rewrite.ReplaceText(sourceRange, caseStatements[i]);

                nextCaseLoc = caseList->getKeywordLoc();
                caseList = caseList->getNextSwitchCase();
                i++;
            }
            //removing space from switch to the first case
            sourceRange.setBegin(switchStatement->getSwitchLoc());
            sourceRange.setEnd(nextCaseLoc.getLocWithOffset(-1));
            Rewrite.RemoveText(sourceRange);

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
        void HandleTranslationUnit(clang::ASTContext &Context) override {
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

    ASTConsumerPointer CreateASTConsumer(clang::CompilerInstance &Compiler,
                                         llvm::StringRef Filename) override {
        TheRewriter.setSourceMgr(Compiler.getSourceManager(), Compiler.getLangOpts());
        return std::make_unique<PointerFinder::Consumer>(TheRewriter);
    }

    bool BeginSourceFileAction(clang::CompilerInstance &Compiler,
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

auto main(int argc, const char *argv[]) -> int {
    using namespace clang::tooling;

    CommonOptionsParser OptionsParser(argc, argv, ToolCategory);
    ClangTool Tool(OptionsParser.getCompilations(),
                   OptionsParser.getSourcePathList());

    auto Action = newFrontendActionFactory<MyFrontendAction>();
    return Tool.run(Action.get());
}
