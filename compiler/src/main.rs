#![feature(linked_list_cursors)]
use std::{collections::LinkedList, fs};

// use llvm::{c_str, LLVMFunctionType, LLVMInt32Type, LLVMWriteBitcodeToFile};

mod ast;
mod lexer;
mod parser;
mod trie;

fn main() {
    // unsafe {
    //     let module = llvm::LLVMModuleCreateWithName(c_str!("Potato"));
    //     let param_types = [LLVMInt32Type(), LLVMInt32Type()];
    //     let return_type = LLVMFunctionType(LLVMInt32Type(), param_types.as_ptr(), 2, false);
    //     let sum = llvm::LLVMAddFunction(module, c_str!("sum"), return_type);

    //     LLVMWriteBitcodeToFile(module, c_str!("out.bc"));
    // }

    let contents = fs::read_to_string("test/test.nc").expect("Unable to read file!");
    let tokens = lexer::lex(&contents).unwrap();
    println!("{:#?}", tokens);
    let mut ltokens = LinkedList::new();
    for tok in &tokens {
        ltokens.push_back(tok);
    }
    let ast = parser::parse_from_tokens(&ltokens).unwrap();
    println!("{:#?}", ast);
}
