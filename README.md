python173
=========

cs173 project

::-> get-structured-python
	
		This is where we add a 'translation' from the types that the parser gives us to
	the types on our syntax language (the ones that we use in the desugarer).
		This is probably where you want to start writing code when you start to implement something new.
		The types from the parser can be seen here: http://docs.python.org/release/3.2.1/library/ast.html
		You can also use the 'syntax' command provided by the ta's to check what's the syntax provided by
	the parser for a certain code.
		I have done the If clause, the BoolOp type, the Compare type, the Str type, Or, Eq, NotEq, Lt,
	LtE, Gt, GtE, Is, IsNot.
		I have also changed the error that it throws when we try to run some code with the 'run' command
	provided by the ta's for an error that actually says that the problem is that we haven't handled a case
	in parsing yet, saying which is case is that.
	
::-> python-primitives

		This file just has a 'pretty' method that we have to update whenever we add some primitive type to our language.
		The primitive types are in the core-syntax file.
		
::-> python-syntax	
		This is where the syntax types (the ones that we have to write the code in the desugarer) are declared.
		I have added PyStr (for strings), PyIf (If clause), PyBoolop (for the boolop type) and PyCompare (for the compare type in the parser).
		You told me that the core language should probably have just a CPrim1 or CPrim2 and you were right, so the compare and the boolop 
	probably end up desugaring into them.
		I'm basically creating one syntax type for every type of statement and expression that the parser 
	gives us.
		Finally, there's a stub case called 'NotExist' that is there just so I can keep an else clause in the desugarer 
	without receiving an error saying that the else cause is unreachable.
	
::-> python-lib
		This is where some functions from libraries are being implemented. For now, just the assert functions that the tests need were written.
	I'm not sure if I wrote all of them, and for now, although they are correctly written (as far as I know, sure), they may use things that have 
	not yet been implemented (like the 'in' case in CPrim2).
	
::-> python-core-syntax
		This is where the primitive types are (the ones that we are returning after everything - they are the analogue for the V values
	in the last assignments, the ones the interpreter return).
		I just added two types for now: VFalse and VNone.
		Also, this is where the core language (the one used in the interpreter) is. I have added CPrim2, CNone and CFalse.
		Finally, there's a stub case called 'NotExist' that is there just so I can keep an else clause in the interpreter 
	without receiving an error saying that the else cause is unreachable.
	
::-> python-interp
		This is where our interpreter is. I have done the CPrim2 case (I used helper methods so the code wouldn't get too long), but I haven't done the 'and' 
	case of it yet. It is probably pretty similar to the 'or' case.
		I have done the CIf clause, the CNone and the CFalse.
		
::-> python-desugar
		This is where... zzzzzZZZzzzz. I have done the PyStr, PyIf, PyBoolop and PyCompare cases, and added an else that prints an 
	error saying what we're missing in the desugarer.
	