#ifndef SYMBOLDATABASE_H_
#define SYMBOLDATABASE_H_

#include <string>
#include <set>
#include "CommonTypes.h"
using namespace std;

class SymbolTableEntry
{
	public:
		SymbolTableEntry(unsigned id, string name, value_type_t type)
			: id_(id), name_(name), type_(type)
		{ }
		unsigned GetId() { return id_; }
		string GetName() { return name_; }
		value_type_t GetType() { return type_; }

	private:
		unsigned id_;
		string name_;
		value_type_t type_;
};

class SymbolTable
{
	public:
		SymbolTable()
		{ }

		SymbolTableEntry* AddSymbol(string name, value_type_t type)
		{
			SymbolTableEntry* p_sym = new SymbolTableEntry(current_id++, name, type);
			table_.insert(p_sym);
			return p_sym;
		}

	private:
		set<SymbolTableEntry*> table_;
		unsigned current_id;
};

#endif // SYMBOLDATABASE_H_
