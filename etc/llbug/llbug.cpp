#include "llvm-c/Analysis.h"
#include "llvm-c/Core.h"
#include "llvm-c/ExecutionEngine.h"
#include "llvm-c/Target.h"
#include "llvm-c/Transforms/PassManagerBuilder.h"
#include "llvm-c/Transforms/Scalar.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/MCJIT.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/Host.h"
#include <iostream>

using namespace llvm;

int main()
{
	LLVMInitializeNativeTarget();
	LLVMInitializeNativeAsmPrinter();
	LLVMInitializeNativeAsmParser();

	char *Error = nullptr;
	std::string HostTriple(sys::getProcessTriple());

	LLVMModuleRef Module = LLVMModuleCreateWithName("simple_module");
	LLVMSetTarget(Module, HostTriple.c_str());

	LLVMValueRef Function = LLVMAddFunction(Module, "scramble",
						LLVMFunctionType(LLVMInt32Type(), nullptr,0, 0));
	LLVMSetFunctionCallConv(Function, LLVMCCallConv);
    	LLVMBasicBlockRef entry = LLVMAppendBasicBlock(Function, "entry");
	LLVMBuilderRef builder = LLVMCreateBuilder();
	LLVMPositionBuilderAtEnd(builder, entry);
	LLVMBuildRet(builder, LLVMConstInt(LLVMInt32Type(), 24, 0));

	Function = LLVMAddFunction(Module, "ooo1",
						LLVMFunctionType(LLVMInt32Type(), nullptr,0, 0));
	LLVMSetFunctionCallConv(Function, LLVMCCallConv);
    	entry = LLVMAppendBasicBlock(Function, "entry");
	builder = LLVMCreateBuilder();
	LLVMPositionBuilderAtEnd(builder, entry);
	LLVMBuildRet(builder, LLVMConstInt(LLVMInt32Type(), 42, 0));

	LLVMDumpModule(Module);
	LLVMVerifyModule(Module, LLVMAbortProcessAction, &Error);
	LLVMDisposeMessage(Error);
	LLVMDisposeBuilder(builder);

	LLVMMCJITCompilerOptions Options;
	LLVMInitializeMCJITCompilerOptions(&Options, sizeof(Options));
	Options.OptLevel = 2;
	Options.NoFramePointerElim = false;
	LLVMExecutionEngineRef Engine;
	if (LLVMCreateMCJITCompilerForModule(&Engine, Module, &Options,
					     sizeof(Options), &Error) != 0) {
		printf("%s\n", Error);
		return 128;
	}
	union {
		uint64_t raw;
		int (*usable)();
	} functionPointer;
	functionPointer.raw = LLVMGetFunctionAddress(Engine, "ooo1");
	assert(functionPointer.raw != 0 && "NULL functionPointer");
	std::cout << functionPointer.usable() << std::endl;
    
	return 0;
}
