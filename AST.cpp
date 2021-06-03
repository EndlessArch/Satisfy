#include "AST.hpp"

#include "Codegen.hpp"
#include "Error.hpp"
#include "Parser.hpp"

#include "tmp.hpp"

#include <llvm/IR/Instructions.h>
#include <llvm/IR/Verifier.h>

namespace satisfy {
namespace ast {

  IdentifierAST::IdentifierAST(void) {}

  IdentifierAST::IdentifierAST(const std::string & _id)
    : idName_(_id) {
  }

  llvm::Value *
  IdentifierAST::codegen(CodeGenContext & _cgc) noexcept {
    auto & loc = _cgc.getLocal();
    if(loc.find(idName_) == loc.end()) {
      err("Undeclared identifier, \'" + idName_ + "\'");
      return nullptr;
    }

    return _cgc.getBuilder().CreateLoad(llvm::Type::getLabelTy(_cgc.getLLVMCtx()),
                                        loc[idName_]);
    // return new llvm::LoadInst(llvm::Type::getLabelTy(_cgc.getLLVMCtx()),
    //                           loc[idName_],
    //                           "",
    //                           false,
    //                           _cgc.currentBlock());
  }

  llvm::Type *
  getTypeOf(llvm::LLVMContext & ctx, const IdentifierAST & id) noexcept {

    if(id.idName_ == "int"
       || id.idName_ == "uint")
      return returnTypeOf(codegen::getBasicInt(ctx));

    if(id.idName_ == "char"
       || id.idName_ == "i8"
       || id.idName_ == "u8")
      return returnTypeOf(llvm::Type::getInt8Ty(ctx));

    if(id.idName_ == "i16"
       || id.idName_ == "u16")
      return returnTypeOf(llvm::Type::getInt16Ty(ctx));

    if(id.idName_ == "i32"
       || id.idName_ == "u32")
      return returnTypeOf(llvm::Type::getInt32Ty(ctx));

    if(id.idName_ == "i64"
       || id.idName_ == "u64")
      return returnTypeOf(llvm::Type::getInt64Ty(ctx));

    if(id.idName_ == "float"
       || id.idName_ == "f32")
      return returnTypeOf(llvm::Type::getFloatTy(ctx));

    if(id.idName_ == "double"
       || id.idName_ == "f64")
      return returnTypeOf(llvm::Type::getDoubleTy(ctx));

    return nullptr;
  }

  bool isFloatingPoint(llvm::LLVMContext & ctx, VariableAST & var) noexcept {
    auto * ty = getTypeOf(ctx, var.varType_);
    return ty == llvm::Type::getFloatTy(ctx)
      || ty == llvm::Type::getDoubleTy(ctx);
  }

  NumberAST::NumberAST(llvm::Type * type, parser::value_type val)
    : targetType_(IdentifierAST("")), val_(val) {
    ;
  }

  NumberAST::NumberAST(IdentifierAST type, parser::value_type val)
    : targetType_(type), val_(val) {
    ;
  }

  llvm::Value *
  NumberAST::codegen(CodeGenContext & _cgc) noexcept {

    // llvm::DataLayout::getTypeAllocSize();
    auto & dl = _cgc.getModule().getDataLayout();

    int bits;
    if(typePtr_)
      bits = dl.getTypeAllocSize(typePtr_);
    else
      bits = dl.getTypeSizeInBits(getTypeOf(_cgc.getLLVMCtx(),
                                              targetType_));

    int sc;

    struct findType_s {
      int operator()(int i) noexcept { return 0; }
      int operator()(unsigned u) noexcept { return 1; }
      int operator()(double d) noexcept { return 2; }
    };

    sc = std::visit(findType_s(), parser::getNumVal());

    // sc = std::visit([](auto a) -> int {
    //   using dt = decltype(a);
    //   if constexpr (std::is_same_v<dt, int>)
    //     return 1;
    //   else if constexpr (std::is_same_v<dt, double>)
    //     return 2;
    //   return 1;
    // }, parser::getNumVal());
    switch(sc) {
    case 0:
      return llvm::ConstantInt::get(_cgc.getLLVMCtx(),
                                    llvm::APInt(bits, std::get<int>(parser::getNumVal()), true));      
    case 1:
      return llvm::ConstantInt::get(_cgc.getLLVMCtx(),
                                    llvm::APInt(bits, std::get<unsigned>(parser::getNumVal()), false));
    case 2:
      return llvm::ConstantFP::get(_cgc.getLLVMCtx(),
                                   llvm::APFloat(std::get<double>(parser::getNumVal())));
    default:
      break;
    }
    return nullptr;
  }

  VariableAST::VariableAST(std::string varName)
    : varType_(""), varName_(varName) {
    ;
  }

  VariableAST::VariableAST(const IdentifierAST & id,
                           std::string varName)
    : varType_(id), varName_(varName) {
    ;
  }
  
  VariableAST::VariableAST(const IdentifierAST & id,
                           std::string varName,
                           SafeExprPtr varAssign)
    : varType_(id), varName_(varName), varAssign_(varAssign) {
    ;
  }

  llvm::Value *
  VariableAST::codegen(CodeGenContext & _cgc) noexcept {
    llvm::Type * varTy = getTypeOf(_cgc.getLLVMCtx(), varType_);
    if(!varTy) {
      satisfy::printErr("Unexpected type identifier \'" + varType_.idName_ + "\'");
      return nullptr;
    }

    llvm::AllocaInst * alloca = _cgc.getBuilder().CreateAlloca(varTy, 0, varName_.c_str());

    // TODO:
    _cgc.getLocal()[varName_.c_str()] = alloca;

    if(varAssign_) {
      // AssignmentAST assign(varName_, varAssign_);
      // assign.codegen(_cgc);
      return varAssign_->codegen(_cgc);
    }

    return _cgc.getBuilder().CreateLoad(varTy,
                                        alloca,
                                        varName_);
  }

  AssignmentAST::AssignmentAST(const std::string & lhs,
                               SafeExprPtr rhs)
    : lhs_(lhs), rhs_(rhs) {
    ;
  }

  llvm::Value *
  AssignmentAST::codegen(CodeGenContext & _cgc) noexcept {
    auto & local = _cgc.getLocal();

    if(local.find(lhs_) == local.end())
      satisfy::printErr("Undefined variable \'" + lhs_ + "\'");

    return _cgc.getBuilder().CreateStore(rhs_->codegen(_cgc),
                                         local[lhs_]);
  }

  ReassignmentAST::ReassignmentAST(const VariableAST & lhs,
                                   SafeExprPtr rhs)
    : lhs_(lhs), rhs_(rhs) {
    ;
  }

  llvm::Value *
  ReassignmentAST::codegen(CodeGenContext & _cgc) noexcept {
    return _cgc.getBuilder().CreateStore(rhs_->codegen(_cgc),
                                         lhs_.codegen(_cgc));
  }

  CodeBlockAST::CodeBlockAST(llvm::BasicBlock * bb,
                             llvm::Function * fp)
    : bb_(bb), exprs_({}), blockName_(""), parent_(fp) {
  }

  CodeBlockAST::CodeBlockAST(std::string _blk,
                             llvm::Function * fp)
    : exprs_({}), blockName_(_blk), parent_(fp) {
  }

  CodeBlockAST::CodeBlockAST(SafeExprPtr _sep, std::string _blk,
                             llvm::Function * fp)
    : blockName_(_blk), parent_(fp) {
    exprs_.push_back(_sep);
  }

  CodeBlockAST::CodeBlockAST(std::initializer_list<SafeExprPtr> _il,
                             std::string _blk,
                             llvm::Function * fp)
    : exprs_(_il), blockName_(_blk), parent_(fp) {
    ;
  }

  llvm::Value *
  CodeBlockAST::codegen(CodeGenContext & _cgc) noexcept {
    llvm::Value * lastVal;

    std::cout << "Found " << exprs_.size() << " ast expressions\n";

    llvm::BasicBlock * currentBlock;
    if(!bb_) {
      llvm::Function * parentPtr = nullptr;
      // remind root code type is CodeBlockAST
      if(!_cgc.isOnBaseCode())
         parentPtr = _cgc.currentBlock()->getParent();
      currentBlock
        = llvm::BasicBlock::Create(_cgc.getLLVMCtx(),
                                   blockName_,
                                   parentPtr);
    } else currentBlock = bb_;

    _cgc.pushBlock(currentBlock);

    auto & localVars = _cgc.getLocal();
    std::string name;

    if(parent_ && !parent_->arg_empty()) {
      for(auto & i : parent_->args()) {
        name = i.getName();
        if(localVars.find(name) == localVars.end())
          localVars[name] = (llvm::Value *)i.getType();
        else {
          printErr((std::string)"There is already declared variable \'"
                   + name + "\'");
          return nullptr;
        }
      }
    }

    ///
    for (auto it = exprs_.begin(); it != exprs_.end(); ++it)
      lastVal = it->get()->codegen(_cgc);
    ///

    _cgc.popBlock();

    return lastVal;
  }

  void CodeBlockAST::push(SafeExprPtr _sep) noexcept {
    if(!exprs_.size()) {
      exprs_ = { _sep };
      return;
    }
    exprs_.push_back(_sep);
  }

  void CodeBlockAST::setBlockName(const std::string & blockName) noexcept {
    blockName_ = blockName;
  }

  void CodeBlockAST::setBlock(llvm::BasicBlock * block) noexcept {
    bb_ = block;
  }

  UnaryOperatorAST::UnaryOperatorAST(UnaryOperator op,
                                     VariableAST & var)
    : op_(op), var_(var) {
    ;
  }

  llvm::Value *
  UnaryOperatorAST::codegen(CodeGenContext & _cgc) noexcept {
    bool isFP = isFloatingPoint(_cgc.getLLVMCtx(), var_);
    llvm::Instruction::BinaryOps instr;
    switch(op_) {
    case UnaryOperator::PreAddOperator:
    case UnaryOperator::PostAddOperator:
      instr = isFP
        ? llvm::Instruction::FAdd
        : llvm::Instruction::Add;
      break;
    case UnaryOperator::PreSubOperator:
    case UnaryOperator::PostSubOperator:
      instr = isFP
        ? llvm::Instruction::FSub
        : llvm::Instruction::Sub;
      break;
    default:
      satisfy::printErr("Unknown unary operator " + std::to_string((int)op_));
    }
    switch(op_) {
    case UnaryOperator::PreAddOperator:
    case UnaryOperator::PreSubOperator:
      { // for local instant variable, 'val' in switch case.
        llvm::Value * val =
          _cgc.getBuilder().CreateBinOp(instr,
                                        var_.codegen(_cgc),
                                        llvm::ConstantInt::get(_cgc.getLLVMCtx(),
                                                               llvm::APInt(sizeof(int) * 8,
                                                                           1)));
        return _cgc.getBuilder().CreateRet(val);
      }
    case UnaryOperator::PostAddOperator:
    case UnaryOperator::PostSubOperator:
      // post operator
      break;
    }

    llvm::Type * curVarTy = getTypeOf(_cgc.getLLVMCtx(),
                                      var_.varType_);

    IdentifierAST curVar(var_.varName_);

    VariableAST tmp(var_.varType_,
                    "___tmp__v");
    SafeExprPtr safeTmpExpr = std::shared_ptr<ExprAST>((ExprAST *)&tmp);

    AssignmentAST assign(var_.varName_, safeTmpExpr);
    SafeExprPtr equalOp = std::shared_ptr<ExprAST>((ExprAST *)&assign);

    equalOp->codegen(_cgc);

    _cgc.getBuilder().CreateBinOp(instr,
                                  curVar.codegen(_cgc),
                                  isFP
                                  ? (llvm::Value *)llvm::ConstantInt::get(_cgc.getLLVMCtx(),
                                                                          llvm::APInt(sizeof(int) * 8,
                                                                                      1))
                                  : (llvm::Value *)llvm::ConstantFP::get(_cgc.getLLVMCtx(),
                                                                         llvm::APFloat(1.0F)));
    return tmp.codegen(_cgc);
  }

  BinaryOperatorAST::BinaryOperatorAST(BinaryOperator op,
                                       SafeExprPtr lhs,
                                       SafeExprPtr rhs,
                                       bool fp)
    : op_(op), lhs_(lhs), rhs_(rhs), hasFP(fp) {
    ;
  }

  llvm::Value *
  BinaryOperatorAST::codegen(CodeGenContext & _cgc) noexcept {
    llvm::Instruction::BinaryOps instr;
    switch(op_) {
    case BinaryOperator::Add:
      instr = hasFP
        ? llvm::Instruction::FAdd
        : llvm::Instruction::Add;
      break;
    case BinaryOperator::Sub:
      instr = hasFP
        ? llvm::Instruction::FSub
        : llvm::Instruction::Sub;
      break;
    case BinaryOperator::Mul:
      instr = hasFP
        ? llvm::Instruction::FMul
        : llvm::Instruction::Mul;
      break;
    case BinaryOperator::Div:
      instr = hasFP
        ? llvm::Instruction::FDiv
        : llvm::Instruction::SDiv;
        // TODO: UDIV
    }
    return llvm::BinaryOperator::Create(instr,
                                        lhs_->codegen(_cgc),
                                        rhs_->codegen(_cgc));
  }

  FunctionAST::FunctionAST(const std::string & funcName,
                           const IdentifierAST & retType,
                           const std::vector<VariableAST> & parTypes,
                           const CodeBlockAST & cb)
    : funcName_(funcName), retType_(retType),
      params_(parTypes), cb_(cb) {
    ;
  }

  llvm::Value *
  FunctionAST::codegen(CodeGenContext & _cgc) noexcept {
    std::vector<llvm::Type *> types(params_.size());
    
    for(int i = 0; i< params_.size(); ++i)
      types[i] = getTypeOf(_cgc.getLLVMCtx(),
                           params_[i].varType_);

    llvm::FunctionType * funcTy
      = llvm::FunctionType::get(getTypeOf(_cgc.getLLVMCtx(),
                                          retType_.idName_),
                                types,
                                false);
    llvm::Function * func
      = llvm::Function::Create(funcTy,
                               llvm::GlobalValue::ExternalLinkage,
                               funcName_,
                               _cgc.getModule());

    std::cout << cb_.getExprSize() << std::endl;

    // just define function
    if(!cb_.getExprSize())
      return func;
    
    // or declare function
    llvm::BasicBlock * entry
      = llvm::BasicBlock::Create(_cgc.getLLVMCtx(),
                                 "entry",
                                 func);

    cb_.setBlock(entry);

    if(llvm::Value * retVal = cb_.codegen(_cgc)) {
      // _cgc.getBuilder().CreateRet(retVal); // FIXME:

      if(!llvm::verifyFunction(*func, &llvm::errs())) {
        // TODO: Analysis...
        // _cgc.getFPM().run(*func);
        return func;
      }
      std::cout << std::endl;
    }

    // erase, invalid function
    func->eraseFromParent();

    return nullptr;
  }

  llvm::Value *
  ReturnAST::codegen(CodeGenContext & _cgc) noexcept {
    struct ret_s {
      CodeGenContext & cgc_;
      ret_s(CodeGenContext & cgc)
        : cgc_(cgc) {}

      llvm::Value * operator()(VariableAST & var) noexcept {
        return cgc_.getLocal()[var.varName_];
      }
      llvm::Value * operator()(NumberAST & num) noexcept {
        return num.codegen(cgc_);
      }
    };
    return _cgc.getBuilder().CreateRet(std::visit(ret_s(_cgc), this->ret_));
  }
} // ns ast
} // ns satisfy
