#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <vector>
#include <set>
#include "semant.h"
#include "utilities.h"

extern int semant_debug;
extern char *curr_filename;

static bool TESTING = false;
static std::ostringstream nop_sstream;
static std::ostream &log = TESTING ? std::cout : nop_sstream;

// C
static Class_ cur_class = nullptr;
static ClassTable* class_table;
// O
static SymbolTable<Symbol, Symbol> attribute_table;
typedef SymbolTable<Symbol, method_class> MethodTable;
static std::map<Symbol, MethodTable> method_tables;
// M = method_tables根据cur_class.Name()找到的MethodTable

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}



ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {
    install_basic_classes();

    log << "check every class name" << std::endl;
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        cur_class = classes->nth(i);                
        Symbol name = cur_class->GetName();        
        log << "class name: " << name << std::endl;        
        
        if (name == SELF_TYPE) {
            semant_error(cur_class) << "Error! SELF_TYPE redeclared!" << std::endl;
        }

       if (classes_.find(name) != classes_.end()) {
            semant_error(cur_class) << "Error! Class " << name << " has been defined!" << std::endl;
            return ;
        } else {
            classes_[name] = cur_class;
        }
    }

    if (classes_.find(Main) == classes_.end()) {
        semant_error() << "Class Main is not defined." << std::endl;
        return ;
    }

    log << "check class inheritance" << std::endl;
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        cur_class = classes->nth(i);                
        Symbol parent_name = cur_class->GetParent(); 
        Symbol name = cur_class->GetName(); 
        
        log << name;               

        while (parent_name != Object && parent_name != name) {
            if (classes_.find(parent_name) == classes_.end()) {
                semant_error(cur_class) << "Error! Cannot find class " << parent_name << std::endl;
                return;
            }

            if (parent_name == Int || parent_name == Str || parent_name == Bool || parent_name == SELF_TYPE) {
                semant_error(cur_class) << "Error! Class " << cur_class->GetName() << " cannot inherit from " << parent_name << std::endl;
                return;
            }

            log << " -> " << parent_name;

            cur_class = classes_[parent_name];
            parent_name = cur_class->GetParent();
        }

        if (parent_name == name) {
            semant_error(cur_class) << "Error! Cycle inheritance!" << std::endl;
            return;
        } else {
            log << " -> " << Object << std::endl;
        }
    }
}

bool ClassTable::CheckInheritance(Symbol ancestor, Symbol child) {
    log << "CheckInheritance##  ancestor: " << ancestor << ", child: " << child << endl;

    if (ancestor == SELF_TYPE) {
        return child == SELF_TYPE;
    }

    if (child == SELF_TYPE) {
        child = cur_class->GetName();
    }
    log << "child = " << child << std::endl;
    
    while (child != ancestor && child != Object) {
        child = classes_[child]->GetParent();
    }
    log << "child = " << child << std::endl;
    return child == ancestor;
}

Symbol ClassTable::FindLca(Symbol type1, Symbol type2) {
    std::vector<Symbol> v1 = GetInheritancePath(type1);
    std::vector<Symbol> v2 = GetInheritancePath(type2);

    Symbol ans = nullptr; 
    for (int i = v1.size() - 1, j = v2.size() - 1; i >= 0 && j >= 0; --i, --j) {
        if (v1[i] == v2[j]) {
            ans = v1[i];
        }
    }
    return ans;
}

std::vector<Symbol> ClassTable::GetInheritancePath(Symbol type) {
    std::vector<Symbol> ans;

    if (type == SELF_TYPE) {
        type = cur_class->GetName();
    }

    log << "GetInheritancePath of " << type << ": ";
    while (type != Object) {
        ans.push_back(type);
        log << type << "->";
        type = classes_[type]->GetParent();
    }
    ans.push_back(Object);
    log << Object << std::endl;
    return ans;
}


void ClassTable::install_basic_classes() {
    log << "install_basic_classes" << std::endl;

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);

    classes_[Object] = Object_class;
    classes_[IO] = IO_class;
    classes_[Int] = Int_class;
    classes_[Bool] = Bool_class;
    classes_[Str] = Str_class;
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 



void method_class::AddMethodToTable(Class_ cls) {
    method_tables[cls->GetName()].addid(name, new method_class(copy_Symbol(name), formals->copy_list(), copy_Symbol(return_type), expr->copy_Expression()));
}

void attr_class::AddAttributeToTable() {
    if (name == self) {
        class_table->semant_error(cur_class->get_filename(), this) << "Error! 'self' cannot be the name of an attribute in class " << cur_class->GetName() << std::endl;
    }

    if (attribute_table.lookup(name) != nullptr) {
        class_table->semant_error(cur_class->get_filename(), this) << "Error! attribute '" << name << "' already exists!" << std::endl;
        return;
    }

    attribute_table.addid(name, new Symbol(type_decl));
}

void method_class::CheckFeatureType() {
    log << "    Checking method \"" << name << "\"" << std::endl;

    std::map<Symbol, Class_> classes = class_table->GetClasses();
    if (return_type != SELF_TYPE && classes.find(return_type) == classes.end()) {
        class_table->semant_error(cur_class->get_filename(), this) << "Error! Cannot find class " << return_type << std::endl;
    }

    attribute_table.enterscope();

    std::set<Symbol> name_set;
    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        Formal formal = formals->nth(i);
        Symbol name = formal->Name(); 
        log << "    formal name: " << name << std::endl;
        if (name == self) {
            class_table->semant_error(cur_class->get_filename(), this) << "Error! self in formal " << std::endl;
        }
        if (name_set.find(name) != name_set.end()) {
            class_table->semant_error(cur_class->get_filename(), this) << "Error! formal name duplicated. " << std::endl;
        }
        name_set.insert(name);

        Symbol type = formal->Type();
        if (classes.find(type) == classes.end()) {
            class_table->semant_error(cur_class->get_filename(), this) << "Error! Cannot find class " << type << std::endl;
        }
        
        attribute_table.addid(name, new Symbol(type));
    }

    Symbol expr_type = expr->EvalExprType();
    log << "    expr_type = " << expr_type << std::endl;
    log << "    return_type = " << return_type << std::endl;
    if (!class_table->CheckInheritance(return_type, expr_type)) {
        class_table->semant_error(cur_class->get_filename(), this) << "Error! lvalue is not an ancestor of rvalue. " << std::endl;
    }

    attribute_table.exitscope();
}

void attr_class::CheckFeatureType() {
    log << "    Checking atribute \"" << name << "\"" << std::endl;

    Symbol type = init->EvalExprType();
    if (type == No_type) {
        log << "NO INIT" << std::endl;
    }
}


Symbol branch_class::EvalCaseType() {
    attribute_table.enterscope();
    attribute_table.addid(name, new Symbol(type_decl));

    Symbol type = expr->EvalExprType();

    attribute_table.exitscope();
    return type;
}

Symbol assign_class::EvalExprType() {
    Symbol* id = attribute_table.lookup(name);
    if (id == nullptr) {
        class_table->semant_error(cur_class->get_filename(), this) << "Error! Cannot find lvalue " << name << std::endl;
        return type = Object;
    }

    type = expr->EvalExprType();
    log << "type = " << type << std::endl;
    if (!class_table->CheckInheritance(*id, type)) {
        class_table->semant_error(cur_class->get_filename(), this) << "Error! lvalue is not an ancestor of rvalue. " << std::endl;
        return Object;
    }
    log << "type = " << type << std::endl;
    return type;
}

Symbol static_dispatch_class::EvalExprType() {
    type = expr->EvalExprType();
    log << "Static Dispatch: class = " << type << ", name = " << name << std::endl;
    if (!class_table->CheckInheritance(type_name, type)) {
        class_table->semant_error(cur_class->get_filename(), this) << "Error! lvalue is not an ancestor of rvalue. " << std::endl;
        return type = Object;
    }

    // find method
    std::vector<Symbol> path = class_table->GetInheritancePath(type_name);
    method_class* method = nullptr;
    for (auto p: path) {
        method = method_tables[p].lookup(name);
        if (method != nullptr) break;
    }
    if (method == nullptr) {
        class_table->semant_error(cur_class->get_filename(), this) << "Error! Cannot find method '" << name << "'" << std::endl;
        return type = Object;
    }

    // check actual list
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        Symbol actual_type = actual->nth(i)->EvalExprType();
        if (method != nullptr) {
            Formal formal = method->GetFormals()->nth(i);
            Symbol formal_type = formal->Type();
            if (!class_table->CheckInheritance(formal_type, actual_type)) {
                class_table->semant_error(cur_class->get_filename(), this) << "Error! lvalue is not an ancestor of rvalue. " << std::endl;
                return type = Object;
            }
        }
    }

    // return type
    Symbol return_type = method->ReturnType();
    if (return_type == SELF_TYPE) {
        return_type = type;
    }
    return type = return_type;
}

Symbol dispatch_class::EvalExprType() {
    type = expr->EvalExprType();
    log << "Dispatch: class = " << type << ", name = " << name << std::endl;

    // find method
    std::vector<Symbol> path = class_table->GetInheritancePath(type);
    method_class* method = nullptr;
    for (auto p: path) {
        method = method_tables[p].lookup(name);
        if (method != nullptr) break;
    }
    if (method == nullptr) {
        class_table->semant_error(cur_class->get_filename(), this) << "Error! Cannot find method '" << name << "'" << std::endl;
        return type = Object;
    }

    int method_formal_len = method->GetFormals()->len();
    if (method_formal_len != actual->len()) {
        class_table->semant_error(cur_class->get_filename(), this) << "Error! method formal len not equal to actual" << std::endl;
        return type = Object;
    }

    // check actual list
    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        Symbol actual_type = actual->nth(i)->EvalExprType();
        if (method != nullptr) {
            Symbol formal_type = method->GetFormals()->nth(i)->Type();
            if (!class_table->CheckInheritance(formal_type, actual_type)) {
                class_table->semant_error(cur_class->get_filename(), this) << "Error! lvalue is not an ancestor of rvalue. " << std::endl;
                return type = Object;
            }
        }
    }

    // return type
    Symbol return_type = method->ReturnType();
    if (return_type == SELF_TYPE) {
        return_type = type;
    }
    return type = return_type;
}

Symbol cond_class::EvalExprType() {
    Symbol pred_type = pred->EvalExprType();
    if (pred_type != Bool) {
        class_table->semant_error(cur_class->get_filename(), this) << "Error! Type of pred is not Bool." << std::endl;
    }

    Symbol then_type = then_exp->EvalExprType();
    Symbol else_type = else_exp->EvalExprType();
    
    if (else_type == No_type) {
        return type = then_type;
    }
    return type = class_table->FindLca(then_type, else_type);
}

Symbol loop_class::EvalExprType() {
    Symbol pred_type = pred->EvalExprType();
    if (pred_type != Bool) {
        class_table->semant_error(cur_class->get_filename(), this) << "Error! Type of pred is not Bool." << std::endl;
    }

    body->EvalExprType();
    return type = Object;
}

Symbol typcase_class::EvalExprType() {
    std::vector<Symbol> types, decl_types;
    expr->EvalExprType();
    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        Case branch = cases->nth(i);
        types.push_back(branch->EvalCaseType());
        decl_types.push_back(((branch_class *) branch)->TypeDecl());
    }

    for (size_t i = 0; i < decl_types.size(); ++i)
        for (size_t j = i + 1; j < decl_types.size(); ++j)
            if (decl_types[i] == decl_types[j]) {
                class_table->semant_error(cur_class->get_filename(), this) << "Error! Two branches have same type." << std::endl;
            }

    type = types[0];
    for (size_t i = 1; i < types.size(); ++i) {
        type = class_table->FindLca(type, types[i]);
    }
    return type;
}

Symbol block_class::EvalExprType() {
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        Expression expr = body->nth(i);
        type = expr->EvalExprType();
    }
    return type;
}

Symbol let_class::EvalExprType() {
    if (identifier == self) {
        class_table->semant_error(cur_class->get_filename(), this) << "Error! self in let binding." << std::endl;
    }

    Symbol init_type = init->EvalExprType();
    if (init_type != No_type) {
        if (!class_table->CheckInheritance(type_decl, init_type)) {
            class_table->semant_error(cur_class->get_filename(), this) << "Error! init value is not child." << std::endl;
        }
    }

    attribute_table.enterscope();
    attribute_table.addid(identifier, new Symbol(type_decl));

    type = body->EvalExprType();

    attribute_table.exitscope();
    return type;
}

Symbol plus_class::EvalExprType() {
    if (e1->EvalExprType() != Int || e2->EvalExprType() != Int) {
        class_table->semant_error(cur_class->get_filename(), this) << "Error! '+' meets non-Int value." << std::endl;
        return Object;
    }
    return type = Int;
}

Symbol sub_class::EvalExprType() {
    if (e1->EvalExprType() != Int || e2->EvalExprType() != Int) {
        class_table->semant_error(cur_class->get_filename(), this) << "Error! '-' meets non-Int value." << std::endl;
        return type = Object;
    }
    return type = Int;
}

Symbol mul_class::EvalExprType() {
    if (e1->EvalExprType() != Int || e2->EvalExprType() != Int) {
        class_table->semant_error(cur_class->get_filename(), this) << "Error! '*' meets non-Int value." << std::endl;
        return type = Object;
    }
    return type = Int;
}

Symbol divide_class::EvalExprType() {
    if (e1->EvalExprType() != Int || e2->EvalExprType() != Int) {
        class_table->semant_error(cur_class->get_filename(), this) << "Error! '/' meets non-Int value." << std::endl;
        return type = Object;
    }
    return type = Int;
}

Symbol neg_class::EvalExprType() {
    if (e1->EvalExprType() != Int) {
        class_table->semant_error(cur_class->get_filename(), this) << "Error! '~' meets non-Int value." << std::endl;
        return type = Object;
    }
    return type = Int;
}

Symbol lt_class::EvalExprType() {
    if (e1->EvalExprType() != Int || e2->EvalExprType() != Int) {
        class_table->semant_error(cur_class->get_filename(), this) << "Error! '<' meets non-Int value." << std::endl;
        return type = Object;
    }
    return type = Bool;
}

Symbol eq_class::EvalExprType() {
    Symbol type1 = e1->EvalExprType(); 
    Symbol type2 = e2->EvalExprType(); 

    if ((type1 == Int || type1 == Str || type1 == Bool || type2 == Int || type2 == Str || type2 == Bool) && (type1 != type2)) {
        class_table->semant_error(cur_class->get_filename(), this) << "Error! '=' meets different types." << std::endl;
        return type = Object;
    }
    return type = Bool;
}

Symbol leq_class::EvalExprType() {
    if (e1->EvalExprType() != Int || e2->EvalExprType() != Int) {
        class_table->semant_error(cur_class->get_filename(), this) << "Error! '<=' meets non-Int value." << std::endl;
        return type = Object;
    }
    return type = Bool;
}

// not
Symbol comp_class::EvalExprType() {
    if (e1->EvalExprType() != Bool) {
        class_table->semant_error(cur_class->get_filename(), this) << "Error! 'not' meets non-Bool value." << std::endl;
        return type = Object;
    }
    return type = Bool;
}

Symbol int_const_class::EvalExprType() {
    return type = Int;
}

Symbol bool_const_class::EvalExprType() {
    return type = Bool;
}

Symbol string_const_class::EvalExprType() {
    return type = Str;
}

Symbol new__class::EvalExprType() {
    std::map<Symbol, Class_> classes = class_table->GetClasses();
    if (type_name != SELF_TYPE && classes.find(type_name) == classes.end()) {
        class_table->semant_error(cur_class->get_filename(), this) << "Error! type " << type_name << " doesn't exist." << std::endl;
    }
    return type = type_name;
}

Symbol isvoid_class::EvalExprType() {
    e1->EvalExprType(); 
    return type = Bool;
}

Symbol no_expr_class::EvalExprType() {
    return type = No_type;
}

Symbol object_class::EvalExprType() {
    if (name == self) {
        return type = SELF_TYPE;
    }

    Symbol* lookup_type = attribute_table.lookup(name);
    if (lookup_type == nullptr) {
        class_table->semant_error(cur_class->get_filename(), this) << "Cannot find object " << name << std::endl;
        return type = Object;
    }
    return type = *lookup_type;
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();

    /* ClassTable constructor may do some semantic analysis */
    class_table = new ClassTable(classes);
    if (class_table->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }
    std::map<Symbol, Class_> all_classes = class_table->GetClasses();

    log << "build method tables" << std::endl;
    for (std::map<Symbol, Class_>::iterator iter = all_classes.begin(); iter != all_classes.end(); ++iter) {
        cur_class = iter->second;
        method_tables[cur_class->GetName()].enterscope();

        Features features = cur_class->GetFeatures();
        for (int i = features->first(); features->more(i); i = features->next(i)) {
            Feature cur_feature = features->nth(i);
            cur_feature->AddMethodToTable(cur_class);
        }
    }
    log << std::endl;

    log << "check illegal override" << std::endl;
    for (std::map<Symbol, Class_>::iterator iter = all_classes.begin(); iter != all_classes.end(); ++iter) {
        cur_class = iter->second;
        Symbol cur_class_name = cur_class->GetName();
        log << "class name: " << cur_class_name << std::endl;
        std::vector<Symbol> path = class_table->GetInheritancePath(cur_class_name);

        Features features = cur_class->GetFeatures();
        for (int j = features->first(); features->more(j); j = features->next(j)) {
            Feature cur_feature = features->nth(j);
            if (!cur_feature->IsMethod()) continue;

            Symbol method_name = ((method_class *)cur_feature)->Name();
            Formals formals = ((method_class *)cur_feature)->GetFormals();

            for (auto p_class_name: path) {
                // 寻找父类同名方法
                method_class* p_method = method_tables[p_class_name].lookup(method_name);

                if (p_method != nullptr) {
                    Formals p_formals = p_method->GetFormals();
                    // 判断参数列表是否一样长
                    if (formals->len() != p_formals->len()) {
                        class_table->semant_error(cur_class->get_filename(), this) << "Method override error: length of formals not match." << std::endl;
                        continue;
                    }
                    // 逐个判断参数类型是否一致
                    for (int k = formals->first(); formals->more(k); k = formals->next(k)) {
                        Formal cur_formal = formals->nth(k);
                        Formal p_cur_formal = p_formals->nth(k);
                        if (cur_formal->Type() != p_cur_formal->Type()) {
                            class_table->semant_error(cur_class->get_filename(), this) << "Method override error: formal type not match." << std::endl;
                        }
                    }
                }
            }

        }
    }
    log << std::endl;

    log << "Now checking all the types:" << std::endl;
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        cur_class = classes->nth(i);
        Symbol class_name = cur_class->GetName();
        log << "cur_class name: " << class_name << std::endl;
        std::vector<Symbol> path = class_table->GetInheritancePath(class_name);

        log << "build attribute table" << std::endl;
        for (auto p : path) {
            attribute_table.enterscope();
            Features features = all_classes[p]->GetFeatures();
            for (int j = features->first(); features->more(j);j = features->next(j)) {
                Feature feature = features->nth(j);
                feature->AddAttributeToTable();
            }
        }

        log << "check features" << std::endl;
        Features features = cur_class->GetFeatures();
        for (int j = features->first(); features->more(j); j = features->next(j)) {
            Feature feature = features->nth(j);
            log << "feature name = " << feature->Name() << std::endl;
            feature->CheckFeatureType();
            log << std::endl;
        }

        for (auto p : path) {
            attribute_table.exitscope();
        }
        log << std::endl;
    }
    log << std::endl;

    if (class_table->errors()) {
	cerr << "Compilation halted due to static semantic errors." << endl;
	exit(1);
    }
}


