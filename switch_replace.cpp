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
#include <string>
#include <vector>
#include <iostream>

bool hasBreak(std::string &str){
    auto n = str.find("break;");
    return !(n == std::string::npos);
}

std::string eraseBreak(std::string &str){
    std::string copy = std::string("");
    copy = str;
    auto n = copy.find("break;");
    if(n != std::string::npos)
        copy.erase(n, 6);
    return copy;
}

std::string rTrim(std::string str){
    std::string copy = std::string("");
    copy = str;
    copy.erase(copy.find_last_not_of(" \n\r\t")+1);
    
    return copy;
}

bool isEmpty(std::string str){
    auto n = str.find_last_not_of(" \n\r\t");
    return n == std::string::npos;
}

namespace SwitchIfElse {

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

        void run(const MatchResult &Result) {
            const auto *switchStmt = Result.Nodes.getNodeAs<clang::SwitchStmt>("switchStmt");
            const clang::SourceManager *SM = Result.SourceManager;

            clang::SourceRange range = switchStmt->getSourceRange();

            std::string replacedTxt;
            std::string condTxt = "(";
            std::string prependTxt;
            bool firstIfPrinted = false;

            // Ukoliko postoji definisana promenljiva u sklopu uslova izdvajamo deklaraciju ispred if-else
            if (const clang::VarDecl *DeclCond = switchStmt->getConditionVariable()) {
                condTxt.append(DeclCond->getNameAsString());
                clang::CharSourceRange condRange = clang::CharSourceRange::getTokenRange(DeclCond->getLocStart(),
                                                                                         DeclCond->getLocEnd());
                std::string defTxt = clang::Lexer::getSourceText(condRange, *SM, Result.Context->getLangOpts()).str();
                prependTxt.append(defTxt).append(";\n");
            } else {
                const auto *Cond = switchStmt->getCond();
                clang::CharSourceRange condRange = clang::CharSourceRange::getTokenRange(Cond->getLocStart(),
                                                                                         Cond->getLocEnd());
                condTxt.append(clang::Lexer::getSourceText(condRange, *SM, Result.Context->getLangOpts()).str());
            }
            condTxt.append(")");


            std::vector<const clang::SwitchCase*> cases;
            std::vector<std::string> caseBodies;
            clang::CharSourceRange scRange = clang::CharSourceRange::getTokenRange(
                    switchStmt->getSwitchCaseList()->getColonLoc().getLocWithOffset(2),
                    switchStmt->getBody()->getLocEnd().getLocWithOffset(-1)
            );
            std::string caseBody = clang::Lexer::getSourceText(scRange, *SM, Result.Context->getLangOpts()).str();
            caseBodies.insert(caseBodies.begin(), 1, caseBody);

            // Prikupljamo case-ove i njihova tela u listu
            // da bismo mogli da prolazimo kroz njih u normalnom poretku
            for (const clang::SwitchCase *sc = switchStmt->getSwitchCaseList(); sc != nullptr; sc = sc->getNextSwitchCase()) {
                cases.insert(cases.begin(), 1, sc);
                if (sc->getNextSwitchCase() != nullptr) {
                    clang::CharSourceRange scRange = clang::CharSourceRange::getTokenRange(
                            sc->getNextSwitchCase()->getColonLoc().getLocWithOffset(2),
                            sc->getKeywordLoc().getLocWithOffset(-1)
                    );
                    caseBody = clang::Lexer::getSourceText(scRange, *SM, Result.Context->getLangOpts()).str();
                    caseBodies.insert(caseBodies.begin(), 1, caseBody);
                }
            }
            int skipNext = 0;
            int i = 0;
            for (const clang::SwitchCase *sc : cases) {
                if (skipNext > 0) {
                    skipNext--;
                    i++;
                    continue;
                }

                // Ako smo vec stavili prvi if, svaki naredni treba da ima else
                if (firstIfPrinted) replacedTxt.append(" else ");


                std::vector<const clang::SwitchCase *> conditions;
                conditions.push_back(sc);
                const clang::SwitchCase * substmt = sc;
                while (llvm::isa<clang::SwitchCase>(substmt->getSubStmt())) {
                    skipNext++;
                    substmt = clang::dyn_cast<clang::SwitchCase>(substmt->getSubStmt());
                    conditions.push_back(substmt);
                }

                // Ukoliko case na kom smo trenutno nije default, nadovezujemo uslove
                if (!llvm::isa<clang::DefaultStmt>(sc)) {
                    replacedTxt.append("if (");
                    bool firstCond = true;
                    for (const clang::SwitchCase *condition : conditions) {
                            if (!firstCond)
                                replacedTxt.append(" || ");
                            auto caseStmt = clang::dyn_cast<clang::CaseStmt>(condition);
                            std::string label = extractStringFromExpr(caseStmt->getLHS(), *SM, Result.Context->getLangOpts());
                            replacedTxt.append(condTxt).append(" == (").append(label).append(")");
                            firstCond = false;
                    }
                    replacedTxt.append(") ");
                }

                // Na kraju navodjenja uslova, dodajemo novi red i telo od case a nakon toga nadovezujemo sva tela dok ne dodjemo do break
                replacedTxt.append("{\n");
                replacedTxt.append(rTrim(eraseBreak(caseBodies[i + skipNext])));

                // Sve dok ne naidjemo na case gde ima break, nadovezujemo telo
                int startBody = i + skipNext;
                while (!hasBreak(caseBodies[startBody])) {
                    // Ukoliko je telo prazno, necemo ga ni nadovezati
                    if (!isEmpty(caseBodies[startBody + 1])) {
                        replacedTxt.append("\n");
                        replacedTxt.append(rTrim(eraseBreak(caseBodies[startBody + 1])));
                    }

                    startBody++;
                }

                replacedTxt.append("\n    }");

                if (!firstIfPrinted) firstIfPrinted = true;
                i++;
            }

            Rewrite.ReplaceText(range, prependTxt.append(replacedTxt));
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
        return std::make_unique<SwitchIfElse::Consumer>(TheRewriter);
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
// namespace SwitchIfElse

namespace {
    llvm::cl::extrahelp MoreHelp("\nChange switch to if-else\n");
    llvm::cl::OptionCategory ToolCategory("SwitchIfElse");
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
