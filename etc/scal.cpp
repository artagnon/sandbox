#include <iostream>
#include <sstream>
#include <string>
#include <cmath>
#include "shared_ptr.cpp"
 
typedef double Value;	// double is now Value, since all values here are double

class Operand{
public:
	virtual Value Apply(Value vX) = 0;	// Pure virtual function, only inherited classes use this
};

typedef SharedPtr<Operand> POperand;		// POperator is a new datatype declaration, it specifies a Pointer Operand
 
class BinaryOperator: public Operand{
public:
	BinaryOperator(POperand poFirst, POperand poSecond):	// BinaryOperator() constructor takes two arguments
		m_poFirst(poFirst),	// Constructor for m_poFirst, member of this class; it is initalised to poFirst
		m_poSecond(poSecond)	// Constructor for m_poSecond, member of this class; it is initalised to poSecond
	{}
 
	POperand GetFirst()	// GetFirst() returns the private member m_poFirst
	{return m_poFirst;}
	POperand GetSecond()	// GetSecond() returns the private member m_poSecond
	{return m_poSecond;}
private:
	POperand m_poFirst;	// Binary operator's first operand
	POperand m_poSecond;	// Binary operator's second operand
};

class Constant: public Operand{	// Constant class is meant to handle numerical or Constant expressions
public:
	Constant(Value vValue):
		m_vValue(vValue)	// Initalise member m_vValue
	{}
 
	virtual Value Apply(Value vX){
		return m_vValue;	// If a value is passed to Apply, it is overridden with m_vValue numerical constant
	}
 
private:
	Value m_vValue;
};
 
class Variable: public Operand{	// To handle variables like x,y
public:
	Variable(){}

	virtual Value Apply(Value vX){
		return vX;	// This is where we replace varibles with a numerical value
	}
};

// Start operator classes
 
class Add: public BinaryOperator{
public:
	Add(POperand poFirst, POperand poSecond):
		BinaryOperator(poFirst, poSecond)
	{}
 
	virtual Value Apply(Value vX){
		return GetFirst()->Apply(vX) + GetSecond()->Apply(vX);	// Application of values should be to operands
	}
};
 
class Subtract: public BinaryOperator{
public:
	Subtract(POperand poFirst, POperand poSecond):
	BinaryOperator(poFirst, poSecond)
	{}
 
	virtual Value Apply(Value vX){
		return GetFirst()->Apply(vX) - GetSecond()->Apply(vX);
	}
};

class Multiply: public BinaryOperator{
public:
	Multiply(POperand poFirst, POperand poSecond):
	BinaryOperator(poFirst, poSecond)
	{}
 
	virtual Value Apply(Value vX){
		return GetFirst()->Apply(vX) * GetSecond()->Apply(vX);
	}
};

class Divide: public BinaryOperator{
public:
	Divide(POperand poFirst, POperand poSecond):
	BinaryOperator(poFirst, poSecond)
	{}
 
	virtual Value Apply(Value vX){
		return GetFirst()->Apply(vX) / GetSecond()->Apply(vX);
	}
};

class Exponentiate: public BinaryOperator{
public:
	Exponentiate(POperand poFirst, POperand poSecond):
	BinaryOperator(poFirst, poSecond)
	{}
 
	virtual Value Apply(Value vX){
		return pow(GetFirst()->Apply(vX), GetSecond()->Apply(vX));
	}
};

// End operator classes

POperand Create(POperand poFirst, char cOperator, POperand poSecond){	// Factory function
	if(cOperator=='+')return new Add(poFirst, poSecond);
	else if(cOperator=='-')return new Subtract(poFirst, poSecond);
	else if(cOperator=='*')return new Multiply(poFirst, poSecond);
	else if(cOperator=='/')return new Divide(poFirst, poSecond);
	else if(cOperator=='^')return new Exponentiate(poFirst, poSecond);
}

Value ReadValue(const char* sData, int& nPos){	// Read a Value from sData, starting at nPos; purpose is to return vValue
	int nEnd = nPos + 1;	// Stop reading at nPos+1
	
	while(1){	// Purpose of the loop is to extract one number sans decimals from sData
		char cCh = sData[nEnd];
		if(!(cCh == '.' || (cCh >= '0' && cCh <= '9'))  ||  cCh == 0) break;
		nEnd++;
	}
	
	std::string sNumber(sData + nPos, nEnd - nPos);	// string sNumber now created the number as a char string	
	Value vValue;
	std::istringstream(sNumber) >> vValue;	// Transfer cahr string to Value alias double, variable vValue
	nPos = nEnd;	// I've read until nEnd and extracted vValue for you
	
	return vValue;
}
 
POperand ReadBinary(const char* sData, int& nPos);	// Prototype, required for ReadUnary
 
POperand ReadUnary(const char* sData, int& nPos){	// nPos belongs to main()
	int cCh = sData[nPos];
 
	if(cCh == '('){
		nPos++;
		POperand poOperator = ReadBinary(sData, nPos);		// Call ReadBinary, which in turn calls us back
		nPos++;
		return poOperator;
	}
	else if(cCh == '-' || cCh == '.' || (cCh >= '0' && cCh <= '9')){	// Literal
		return POperand(new Constant(ReadValue(sData, nPos)));		// Constant and Variables are like objects
	}
	else if(cCh == 'x'){
		nPos++;
		return POperand(new Variable());
	}
	else{
		// Bad! Stop parsing here
		exit(0); // Not for production use!
	}
}
 
/*
1 Read the first element (unary expression)
2 Read the next element (operator)


3.0	If the operator is *, read the next operand (unary expression)
3.1	Create a unary expression with (first unary expression)(operator)(second unary expression)
3.2	Set that expression to (first unary expression) and goto 2


3'.0	If the operator is +, read an operand (unary expression)
3'.1	Read an operator


3'.1.1.0		If the operator is *
3'.1.1.1		Put back the second unary expression
3'.1.1.2		Call ReadBinary, (recursive), which returns the new (second unary expression)
3'.1.1.3		Goto 3'.1.1'

3'.1.1'.0		If the operator is +
3'.1.1'.1		Create a unary expression with (first unary expression)(operator)(second unary expression)
*/

inline int Level(const int &cOperator){
	if(cOperator == '+' || cOperator == '-')return 1;
	else if(cOperator == '*' || cOperator =='/')return 2;
	else if(cOperator == '^')return 3;
}
 
POperand ReadBinary(const char* sData, int& nPos){	// nPos belongs to main()
	POperand poFirst = ReadUnary(sData, nPos);	// Do not confuse with the m_ counterparts
	POperand poSecond;

	while(nPos < strlen(sData)){
		int cFirstOperator = sData[nPos];		// cFirstOperator expects an operator
		nPos++;
		int nOldPos = nPos;				// We might have to fall back on the old value
		poSecond = ReadUnary(sData, nPos);

		int cSecondOperator = sData[nPos];
		if(Level(cFirstOperator) < Level(cSecondOperator)){
			nPos = nOldPos;				// Put back poSecond
			poSecond = ReadBinary(sData, nPos);	// Recursion
		}
		poFirst = Create(poFirst, cFirstOperator, poSecond);
		continue;
	}
	return poFirst;
}
 
int main(void){ 
	std::cout << "Enter expression to evaluate: ";
	std::string sValue;
	std::getline(std::cin, sValue);		// The reading from stdin is done here
	
	int nPos = 0;				//nPos belongs to main(), always passed by reference
	Value vApplyValue;
	std::cout << "Enter value to replace x with (if applicable): ";
	std::cin >> vApplyValue;
	std::cout << (ReadBinary(sValue.c_str(), nPos)->Apply(vApplyValue)) << std::endl;
	
	return 0;
}

