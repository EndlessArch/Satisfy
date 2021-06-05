#include "AST.hpp"

#include <llvm/IR/Dominators.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Verifier.h>

#include "Codegen.hpp"
#include "Error.hpp"
#include "Parser.hpp"
#include "tmp.hpp"

namespace satisfy {
namespace ast {

IdentifierAST::IdentifierAST(void) {}

IdentifierAST::IdentifierAST(const std::string& _id) : idName_(_id) {}

llvm::Value* IdentifierAST::codegen(CodeGenContext& _cgc) noexcept {
  auto& loc = _cgc.getLocal();
  if (loc.find(idName_) == loc.end()) {
    err("Undeclared identifier, \'" + idName_ + "\'");
    return nullptr;
  }

  // TOOD: test

  return _cgc.getBuilder().CreateLoad(llvm::Type::getLabelTy(_cgc.getLLVMCtx()),
                                      loc[idName_]);
  // return new llvm::LoadInst(llvm::Type::getLabelTy(_cgc.getLLVMCtx()),
  //                           loc[idName_],
  //                           "",
  //                           false,
  //                           _cgc.currentBlock());
}

llvm::Type* getTypeOf(llvm::LLVMContext& ctx,
                      const IdentifierAST& id) noexcept {
  if (id.idName_ == "int" || id.idName_ == "uint")
    return returnTypeOf(codegen::getBasicInt(ctx));

  if (id.idName_ == "char" || id.idName_ == "i8" || id.idName_ == "u8")
    return returnTypeOf(llvm::Type::getInt8Ty(ctx));

  if (id.idName_ == "i16" || id.idName_ == "u16")
    return returnTypeOf(llvm::Type::getInt16Ty(ctx));

  if (id.idName_ == "i32" || id.idName_ == "u32")
    return returnTypeOf(llvm::Type::getInt32Ty(ctx));

  if (id.idName_ == "i64" || id.idName_ == "u64")
    return returnTypeOf(llvm::Type::getInt64Ty(ctx));

  if (id.idName_ == "float" || id.idName_ == "f32")
    return returnTypeOf(llvm::Type::getFloatTy(ctx));

  if (id.idName_ == "double" || id.idName_ == "f64")
    return returnTypeOf(llvm::Type::getDoubleTy(ctx));

  return nullptr;
}

bool isFloatingPoint(llvm::LLVMContext& ctx, VariableAST& var) noexcept {
  auto* ty = getTypeOf(ctx, var.varType_);
  return ty == llvm::Type::getFloatTy(ctx) ||
         ty == llvm::Type::getDoubleTy(ctx);
}

NumberAST::NumberAST(llvm::Type* type, parser::value_type val)
    : targetType_(IdentifierAST("")), val_(val) {
  ;
}

NumberAST::NumberAST(IdentifierAST type, parser::value_type val)
    : targetType_(type), val_(val) {
  ;
}

llvm::Value* NumberAST::codegen(CodeGenContext& _cgc) noexcept {
  // llvm::DataLayout::getTypeAllocSize();
  auto& dl = _cgc.getModule().getDataLayout();

  int bits;
  if (typePtr_)
    bits = dl.getTypeAllocSize(typePtr_);
  else
    bits = dl.getTypeSizeInBits(getTypeOf(_cgc.getLLVMCtx(), targetType_));

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
  switch (sc) {
    case 0:
      return llvm::ConstantInt::get(
          _cgc.getLLVMCtx(),
          llvm::APInt(bits, std::get<int>(parser::getNumVal()), true));
    case 1:
      return llvm::ConstantInt::get(
          _cgc.getLLVMCtx(),
          llvm::APInt(bits, std::get<unsigned>(parser::getNumVal()), false));
    case 2:
      return llvm::ConstantFP::get(
          _cgc.getLLVMCtx(),
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

VariableAST::VariableAST(const IdentifierAST& id, std::string varName)
    : varType_(id), varName_(varName) {
  ;
}

VariableAST::VariableAST(const IdentifierAST& id,
                         std::string varName,
                         SafeExprPtr varAssign)
    : varType_(id), varName_(varName), varAssign_(varAssign) {
  ;
}

llvm::Value* VariableAST::codegen(CodeGenContext& _cgc) noexcept {
  llvm::Type* varTy;
  if(compileTimeType_)
    varTy = compileTimeType_;
  else {
    if(varType_.idName_.empty()) {
      if(_cgc.getLocal()[varName_])
        return _cgc.getLocal()[varName_];
      else {
        printErr("Unexpected type identifier \'" +
                 varType_.idName_ + "\'");
      }
    } else
      varTy = getTypeOf(_cgc.getLLVMCtx(), varType_);
    // varTy = getTypeOf(_cgc.getLLVMCtx(), varType_);
    
    // if (!varTy) {
    //   if(_cgc.getLocal()[varName_])
    //     varTy = _cgc.getLocal()[varName_]->getType();
    //   else {
    //     satisfy::printErr("Unexpected type identifier \'" + varType_.idName_ +
    //                       "\'");
    //     return nullptr;
    //   }
    // }
  }
  
  // std::cout << "Creating alloca: " << varType_.idName_ << ": " << varName_
  // << std::endl;

  llvm::AllocaInst* alloca =
      _cgc.getBuilder().CreateAlloca(varTy, 0, varName_);

  // TODO:
  _cgc.getLocal()[varName_] = alloca;
  std::cout << varName_ << std::endl;
  if (varAssign_) {
    // AssignmentAST assign(varName_, varAssign_);
    // assign.codegen(_cgc);
    return varAssign_->codegen(_cgc);
  }

  return _cgc.getBuilder().CreateLoad(alloca);

  // return alloca;
}

AssignmentAST::AssignmentAST(const std::string& lhs, SafeExprPtr rhs)
    : lhs_(lhs), rhs_(rhs) {
  ;
}

llvm::Value* AssignmentAST::codegen(CodeGenContext& _cgc) noexcept {
  auto& local = _cgc.getLocal();

  if (local.find(lhs_) == local.end())
    satisfy::printErr("Undefined variable \'" + lhs_ + "\'");

  return _cgc.getBuilder().CreateStore(rhs_->codegen(_cgc), local[lhs_]);
}

ReassignmentAST::ReassignmentAST(const VariableAST& lhs, SafeExprPtr rhs)
    : lhs_(lhs), rhs_(rhs) {
  ;
}

llvm::Value* ReassignmentAST::codegen(CodeGenContext& _cgc) noexcept {
  return _cgc.getBuilder().CreateStore(rhs_->codegen(_cgc), lhs_.codegen(_cgc));
}

CodeAST::CodeAST(void) : exprs_({}) {}

CodeAST::CodeAST(SafeExprPtr _sep) : exprs_({}) {
  exprs_.push_back(_sep);
}

CodeAST::CodeAST(std::initializer_list<SafeExprPtr> il) : exprs_(il) {}

llvm::Value* CodeAST::codegen(CodeGenContext& _cgc) noexcept {
  std::cout << "Found " << exprs_.size() << " pieces of codes\n";

  for (auto& i : exprs_)
    i->codegen(_cgc);

  return nullptr;
}

void CodeAST::push(SafeExprPtr _sep) noexcept {
  exprs_.push_back(_sep);
}

CodeBlockAST::CodeBlockAST(std::string _blk)
    : CodeAST(), blockName_(_blk) {}

CodeBlockAST::CodeBlockAST(SafeExprPtr _sep,
                           std::string _blk)
    : CodeAST(_sep), blockName_(_blk) {}

CodeBlockAST::CodeBlockAST(std::initializer_list<SafeExprPtr> _il,
                           std::string _blk)
    : CodeAST(_il), blockName_(_blk) {
  ;
}

llvm::Value* CodeBlockAST::codegen(CodeGenContext& _cgc) noexcept {
  llvm::Value* lastVal;

  std::cout << "Found " << exprs_.size() << " ast expressions\n";

  // _cgc.pushBlock(currentBlock);

  auto& localVars = _cgc.getLocal();
  std::string name;
  
  ///
  for (auto it = exprs_.begin(); it != exprs_.end(); ++it)
    lastVal = it->get()->codegen(_cgc);
  ///

  // _cgc.popBlock();

  return lastVal;
}

void CodeBlockAST::push(SafeExprPtr _sep) noexcept {
  if (!exprs_.size()) {
    exprs_ = {_sep};
    return;
  }
  exprs_.push_back(_sep);
}

void CodeBlockAST::setBlockName(const std::string& blockName) noexcept {
  blockName_ = blockName;
}

UnaryOperatorAST::UnaryOperatorAST(UnaryOperator op, VariableAST& var)
    : op_(op), var_(var) {
  ;
}

llvm::Value* UnaryOperatorAST::codegen(CodeGenContext& _cgc) noexcept {
  bool isFP = isFloatingPoint(_cgc.getLLVMCtx(), var_);
  llvm::Instruction::BinaryOps instr;
  switch (op_) {
    case UnaryOperator::PreAddOperator:
    case UnaryOperator::PostAddOperator:
      instr = isFP ? llvm::Instruction::FAdd : llvm::Instruction::Add;
      break;
    case UnaryOperator::PreSubOperator:
    case UnaryOperator::PostSubOperator:
      instr = isFP ? llvm::Instruction::FSub : llvm::Instruction::Sub;
      break;
    default:
      satisfy::printErr("Unknown unary operator " + std::to_string((int)op_));
  }
  switch (op_) {
    case UnaryOperator::PreAddOperator:
    case UnaryOperator::PreSubOperator: {  // for local instant variable, 'val'
      // in switch case.
      llvm::Value* val = _cgc.getBuilder().CreateBinOp(
          instr, var_.codegen(_cgc),
          llvm::ConstantInt::get(_cgc.getLLVMCtx(),
                                 llvm::APInt(sizeof(int) * 8, 1)));
      return _cgc.getBuilder().CreateRet(val);
    }
    case UnaryOperator::PostAddOperator:
    case UnaryOperator::PostSubOperator:
      // post operator
      break;
  }

  llvm::Type* curVarTy = getTypeOf(_cgc.getLLVMCtx(), var_.varType_);

  IdentifierAST curVar(var_.varName_);

  VariableAST tmp(var_.varType_, "___tmp__v");
  SafeExprPtr safeTmpExpr = std::shared_ptr<ExprAST>((ExprAST*)&tmp);

  AssignmentAST assign(var_.varName_, safeTmpExpr);
  SafeExprPtr equalOp = std::shared_ptr<ExprAST>((ExprAST*)&assign);

  equalOp->codegen(_cgc);

  _cgc.getBuilder().CreateBinOp(
      instr, curVar.codegen(_cgc),
      isFP ? (llvm::Value*)llvm::ConstantInt::get(
                 _cgc.getLLVMCtx(), llvm::APInt(sizeof(int) * 8, 1))
           : (llvm::Value*)llvm::ConstantFP::get(_cgc.getLLVMCtx(),
                                                 llvm::APFloat(1.0F)));
  return tmp.codegen(_cgc);
}

BinaryOperatorAST::BinaryOperatorAST(std::string op, SafeExprPtr lhs,
                                     SafeExprPtr rhs)
    : lhs_(lhs), rhs_(rhs) {
  if(op == "+")
    op_ = BinaryOperator::Add;
  else if(op == "-")
    op_ = BinaryOperator::Sub;
  else if(op == "*")
    op_ = BinaryOperator::Mul;
  else if(op == "|")
    op_ = BinaryOperator::Div;
  else if(op == "=")
    op_ = BinaryOperator::Set;
  else
    printErr((std::string)"Unknown operator \'" + op + "\'");
}

BinaryOperatorAST::BinaryOperatorAST(BinaryOperator op,
                                     SafeExprPtr lhs,
                                     SafeExprPtr rhs)
    : op_(op), lhs_(lhs), rhs_(rhs) {
  ;
}

llvm::Value* BinaryOperatorAST::codegen(CodeGenContext& _cgc) noexcept {
  bool hasFP = false;

  llvm::Value * lv;
  auto * rv = rhs_->codegen(_cgc);

  auto * v = reinterpret_cast<VariableAST *>(lhs_.get());
  if(!v->varName_.empty()) // is variable
    v->setType(rv->getType());
  ;

  lv = v->codegen(_cgc);

  hasFP = lv->getType()->isFloatTy()
          || rv->getType()->isFloatTy();

  llvm::Instruction::BinaryOps instr;
  switch (op_) {
    case BinaryOperator::Add:
      instr = hasFP ? llvm::Instruction::FAdd : llvm::Instruction::Add;
      break;
    case BinaryOperator::Sub:
      instr = hasFP ? llvm::Instruction::FSub : llvm::Instruction::Sub;
      break;
    case BinaryOperator::Mul:
      instr = hasFP ? llvm::Instruction::FMul : llvm::Instruction::Mul;
      break;
    case BinaryOperator::Div:
      instr = hasFP ? llvm::Instruction::FDiv : llvm::Instruction::SDiv;
      // TODO: UDIV
    case BinaryOperator::Set:
      llvm::AllocaInst * addr
          = _cgc.getBuilder().CreateAlloca(rv->getType(), (unsigned)0);
      _cgc.getBuilder().CreateStore(rv,
                                    addr);
      return _cgc.getBuilder().CreateLoad(addr);
  }
  
  // return _cgc.getBuilder().CreateLoad(
  //     _cgc.getBuilder().CreateBinOp(instr,
  //                                   lv, rv));
  return _cgc.getBuilder().CreateBinOp(instr,
                                       lv, rv);
}

FunctionAST::FunctionAST(const std::string& funcName,
                         const IdentifierAST& retType,
                         const std::vector<VariableAST>& parTypes,
                         const std::optional<CodeBlockAST> & cb)
    : funcName_(funcName), retType_(retType), params_(parTypes), cb_(cb) {
  ;
}

llvm::Value* FunctionAST::codegen(CodeGenContext& _cgc) noexcept {
  std::vector<llvm::Type*> types(params_.size());

  for (int i = 0; i < params_.size(); ++i)
    types[i] = getTypeOf(_cgc.getLLVMCtx(), params_[i].varType_);

  llvm::FunctionType* funcTy = llvm::FunctionType::get(
      getTypeOf(_cgc.getLLVMCtx(), retType_.idName_), types, false);
  llvm::Function* func = llvm::Function::Create(
      funcTy, llvm::GlobalValue::ExternalLinkage, funcName_, _cgc.getModule());

  // just define function
  if (!cb_.has_value())
    return func;

  // or declare function
  llvm::BasicBlock* entry =
      llvm::BasicBlock::Create(_cgc.getLLVMCtx(), "entry", func);

  _cgc.pushBlock(entry);

  // make parameters visible
  for (int i = 0; i < params_.size(); ++i) {
    (func->arg_begin() + i)->setName(params_[i].varName_);
    _cgc.getLocal()[params_[i].varName_] = (func->arg_begin() + i);
  }


  if (llvm::Value* retVal = cb_->codegen(_cgc)) {

    if(!llvm::dyn_cast<llvm::ReturnInst>(retVal)) {
      std::cout << "No any return at the end of the function!\n";
      _cgc.getBuilder().CreateRetVoid();
    }

    std::cout << "\"\n";
    _cgc.getModule().print(llvm::errs(), nullptr);
    std::cout << "\"\n";

    if (!llvm::verifyFunction(*func, &llvm::errs())) {
      // TODO: Analysis...
      // _cgc.getFPM().run(*func);
      _cgc.popBlock();
      return func;
    }
  }

  _cgc.popBlock();

  // erase, invalid function
  func->eraseFromParent();

  return nullptr;
}

ReturnAST::ReturnAST(SafeExprPtr sep)
    : ret_(sep) {
}

llvm::Value* ReturnAST::codegen(CodeGenContext& _cgc) noexcept {
  return _cgc.getBuilder().CreateRet(
      _cgc.getBuilder().CreateLoad(ret_->codegen(_cgc)));
  // return _cgc.getBuilder().CreateRet(ret_->codegen(_cgc));
}

IfAST::IfAST(SafeExprPtr _if, CodeBlockAST _then) : if_(_if), then_(_then) {}

void IfAST::pushElse(CodeBlockAST & _cb) noexcept {
  else_ = _cb;
}

llvm::Value * IfAST::codegen(CodeGenContext & _cgc) noexcept {
  auto * cond = if_->codegen(_cgc);
  
  auto * condV =
      _cgc.getBuilder().CreateICmpNE(cond, llvm::ConstantInt::get(_cgc.getLLVMCtx(),
                                                                  llvm::APInt(sizeof(int) * 8,
                                                                              0)));
  llvm::BasicBlock * bThen, * bElse, * bMerge;

  bThen = llvm::BasicBlock::Create(_cgc.getLLVMCtx(),
                                   "then",
                                   _cgc.currentBlock()->getParent());

  bElse = llvm::BasicBlock::Create(_cgc.getLLVMCtx(),
                                   "else");

  bMerge = llvm::BasicBlock::Create(_cgc.getLLVMCtx(),
                                    "ifcont");

  auto * brV = _cgc.getBuilder().CreateCondBr(condV, bThen, bElse);

  // create 'then'
  _cgc.getBuilder().SetInsertPoint(bThen);
  llvm::Value * thenV = this->then_.codegen(_cgc);

  if(auto & lastInstr = _cgc.getBuilder().GetInsertBlock()->getInstList().back();
     // !llvm::dyn_cast<llvm::ReturnInst>(&lastInstr)
     !lastInstr.isTerminator()) {
    // create 'ifcont'
    _cgc.getBuilder().CreateBr(bMerge);
  }
  // get end of 'then' block
  bThen = _cgc.getBuilder().GetInsertBlock();

  ;

  _cgc.currentBlock()->getParent()->getBasicBlockList().push_back(bElse);
  _cgc.getBuilder().SetInsertPoint(bElse);
  llvm::Value * elseV = this->else_.codegen(_cgc);

  if (auto &lastInstr =
          _cgc.getBuilder().GetInsertBlock()->getInstList().back();
      !lastInstr.isTerminator()) {
    // create 'ifcont'
    _cgc.getBuilder().CreateBr(bMerge);
  }

  bElse = _cgc.getBuilder().GetInsertBlock();

  ;
  
  _cgc.currentBlock()->getParent()->getBasicBlockList().push_back(bMerge);
  _cgc.getBuilder().SetInsertPoint(bMerge);

  return bMerge;
}

}  // namespace ast
}  // namespace satisfy
