#include "everywhere.h"
int g_size_of[]={
	0,
	4,4,8,1,2,4,8,1,2,4,8,16,1,1,
	2,4,8,161,8,0,-1,-1,-1,8,8,8
};
int g_raw_types[]={
	4|RT_SIGNED|RT_INTEGER,
	4|RT_INTEGER,
	8|RT_INTEGER,
	1|RT_SIGNED|RT_INTEGER,
	2|RT_SIGNED|RT_INTEGER,
	4|RT_SIGNED|RT_INTEGER,
	8|RT_SIGNED|RT_INTEGER,
	1|RT_INTEGER,
	2|RT_INTEGER,
	4|RT_INTEGER,
	8|RT_INTEGER,
	16|RT_INTEGER,
	1|RT_INTEGER,//bool
	1|RT_INTEGER,// regbool
	2|RT_FLOATING,//half
	4|RT_FLOATING,
	8|RT_FLOATING,
	16|RT_FLOATING|RT_SIMD,
	1|RT_INTEGER,
	8|RT_POINTER,
	0|0,
	8|RT_POINTER,
	0|0,
	0|0,
	8|RT_POINTER,
	1|RT_INTEGER,
	1|RT_INTEGER,
	0|0,//auto
	8|RT_POINTER,//ptr
	8|RT_POINTER,//ref
	0|0,
};
const char* g_token_str[]={
	"",
	"int","uint","size_t","i8","i16","i32","i64","u8","u16","u32","u64","u128","bool","reg_bool",
	"half","float","double","float4",
	"char","str","void","voidptr","one","zero","nullptr","true","false",
	"auto","ptr","ref","Self",
	"tuple","__NUMBER__","__TYPE__","__IDNAME__",
	
	"print___","fn","struct","class","trait","impl","virtual","static","extern", "enum","array","vector","union","variant","with","match","where","sizeof","typeof","nameof","offsetof", "this","self","super","vtableof","closure",
	"let","var",
	"const","mut","volatile",
	"while","if","else","do","for","in","return","break","continue",
	"(",")",
	"{","}",
	"[","]",
	"<[","]>",
	"->",".","?.","=>","<-","::","<->",			//arrows,accessors
	"|>","<|","<.>","<$>","<:",":>",			// some operators we might nick from other langs
	
	":","as","new","delete",
	"+","-","*","/",					//arithmetic
	"&","|","^","%","<<",">>","?:","?>","?<",					//bitwise
	"<",">","<=",">=","==","!=",		//compares
	"&&","||",		//logical
	"=",":=","=:","@","",".:=",
	"+=","-=","*=","/=","&=","|=","^=","%=","<<=",">>=", // assign-op
	".=","?=",	// linklist follow ptr.=next
	"++","--","++","--", //inc/dec
	"-","*","&","!","~","?", // unary ops
	"&&","*?","*!","&?","~[]","[]","&[]","??","<?>","<*>","<+>","<-->","</>","?->", // special pointers?
	",",";",";;",
	"...","..","..<","..>","..>=","..<=",">..",">=..","<..<",">..>",
	"\'","_","",
	"\"C\"","__vtable_ptr","__data_ptr","__parent_ptr","__env_ptr","__discriminant","__env_i8_ptr","__vector","__string","__unique_ptr","__dictionary","__gc_ptr","__destructor","__set_default_values","__visit","__verify","drop",
	NULL,
};
const char* g_operator_symbol[]={
	"tuple_constructor","","intializer_list","","array_initializer","",
	"tparam_begin","tparam_end",
	"arrow","dot","if_dot","fat_arrow","rev_arrow","in_scope","swap",
	"pipe","rev_pipe","tag_dot","tag_dollar","supertype","subtype",
	"of_type","as","new","delete",
	"op_add","op_sub","op_mul","op_div",
	"bit_and","bit_or","bit_xor","op_mod","op_shl","op_shr","if_else","op_max","op_min","op_lt","op_gt","op_le","op_ge","op_eq","op_ne",
	"log_and","log_or",
	"assign","let_assign","assign_colon","pattern_bind","field_assign",
	"assign_add","assign_sub","assign_mul","assign_div","assign_and","assign_or","assign_xor","assign_mod","assign_shl","assign_shr",
	"assign_dot","maybe_assign",
	"post_inc","post_dec","pre_inc","pre_dec",
	"negate","deref","not","complement","option",
	"rvalue_ref","opt_ptr","own_ptr","maybe_ref","own_vector","slice","slice_ref", "double_question_mark","tag_question_mark","tag_mul","tag_add","tag_sub","tag_div","maybe_arrow",
	"comma","semicolon","doublesemicolon","ellipsis","dotdot",
	"range_lt","range_gt","range_ge","range_le","gt_range","ge_range","lt_range_lt","gt_range_gt",
	"lifetime","placeholder",""
};


int g_tok_info[]={
	0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,// int types
	0,0,0,0,  //floats
	0,0,0,0,0,0,0,0,0,
	0,0,0,0,
	0,0,0,0,			// type modifiers
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0, 0,0,0,0,0, // keywords
	0,0,			// let,var
	0,0,0,			// modifiers const,mut,volatile
	0,0,0,0,0,0,0,0,0,  // while,if,else,do,for,in,return,break
	0,0, //( )
	0,0, //{ }
	0,0, // [ ]
	0,0, //<[Type]>
	READ|10,READ|2,READ|2,READ|10,READ|10,READ|13,WRITE|10,	   // dots, arrows
	READ|17,READ|17,READ|17,READ|5,READ|17,READ|17,	// unusual stuff
	READ|9,READ|9, READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3,
	READ|6,READ|6,READ|5,READ|5,		//arithmetic
	READ|8,READ|7,READ|8,READ|6,READ|9,READ|9,READ|9,READ|9,READ|9,	//bitwise
	READ|8,READ|8,READ|8,READ|8,READ|9,READ|9,	// COMPARES
	READ|13,READ|14,	//logical
	WRITE_LHS|READ_RHS|ASSOC|16,WRITE_LHS|READ_RHS|ASSOC|16,WRITE_LHS|READ_RHS|ASSOC|16,0,0,WRITE_LHS|READ_RHS|ASSOC|16, // assignment
	
	WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16,WRITE_LHS|READ|ASSOC|16, // assign-op
	WRITE_LHS|READ|ASSOC|16, // dot-assign
	MODIFY|PREFIX|UNARY|2,MODIFY|PREFIX|UNARY|2,MODIFY|UNARY|ASSOC|3,MODIFY|UNARY|ASSOC|3, // post/pre inc/dec
	READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3,READ|UNARY|PREFIX|3, //unary ops
	READ|UNARY|ASSOC|3,READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3, READ|UNARY|ASSOC|3,READ|UNARY|ASSOC|3, /// special pointers
	0,0,17, // delim
	0,
	0,0,0,0,
	0,0, //placeholder
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
};
