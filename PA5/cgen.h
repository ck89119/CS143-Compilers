#include <assert.h>
#include <stdio.h>
#include <vector>
#include <map>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;
   std::vector<CgenNode*> class_nodes;
   std::map<Symbol, int> name_tag_map;

   void InitNameTagMap();

// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_select_gc();
   void code_bools(int);
   void code_constants();
   void code_class_name_tab();
   void code_class_obj_tab();
   void code_dispatch_tabs();
   void code_prot_objs();
   void code_global_text();
   void code_class_inits();
   void code_class_methods();

   // The following creates an inheritance graph from
   // a list of classes.  The graph is implemented as
   // a tree of `CgenNode', and class names are placed
   // in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
   std::vector<CgenNode*> GetClassNodes() { return class_nodes; }
   std::map<Symbol, int> GetNameTagMap() { return name_tag_map; }
   CgenNode* GetClassNode(Symbol name) { return class_nodes[name_tag_map[name]]; }
};


class CgenNode : public class__class {
 private:
  CgenNodeP parentnd;        // Parent of class
  List<CgenNode>* children;  // Children of class
  Basicness basic_status;    // `Basic' if class is basic
                             // `NotBasic' otherwise

 public:
  CgenNode(Class_ c, Basicness bstatus, CgenClassTableP class_table);

  void add_child(CgenNodeP child);
  List<CgenNode>* get_children() { return children; }
  void set_parentnd(CgenNodeP p);
  CgenNodeP get_parentnd() { return parentnd; }
  int basic() { return (basic_status == Basic); }

  void code_prot_obj(ostream& s);
  void code_init(ostream& s);
  void code_methods(ostream& s);

  std::vector<method_class*> GetMethods();
  std::vector<method_class*> m_methods;

  std::vector<method_class*> GetFullMethods();
  std::vector<method_class*> m_full_methods;

  std::map<Symbol, Symbol> GetDispatchClassTab();
  std::map<Symbol, Symbol> m_dispatch_class_tab;

  std::map<Symbol, int> GetDispatchIdxTab();
  std::map<Symbol, int> m_dispatch_idx_tab;

  std::vector<attr_class*> GetAttribs();
  std::vector<attr_class*> m_attribs;

  std::vector<attr_class*> GetFullAttribs();
  std::vector<attr_class*> m_full_attribs;

  std::map<Symbol, int> GetAttribIdxTab();
  std::map<Symbol, int> m_attrib_idx_tab;

  std::vector<CgenNode*> GetInheritance();
  std::vector<CgenNode*> inheritance;

  int class_tag;
};

class BoolConst {
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};

class Environment {
 public:
  Environment() : m_class_node(nullptr) {}

  void EnterScope() { m_scope_lengths.push_back(0); }

  void ExitScope() {
    for (int i = 0; i < m_scope_lengths[m_scope_lengths.size() - 1]; ++i) {
      m_var_idx_tab.pop_back();
    }
    m_scope_lengths.pop_back();
  }

  int LookUpAttrib(Symbol sym) {
    std::map<Symbol, int> attrib_idx_tab = m_class_node->GetAttribIdxTab();
    if (attrib_idx_tab.find(sym) != attrib_idx_tab.end()) {
      return attrib_idx_tab[sym];
    }
    return -1;
  }

  // The vars are in reverse order.
  int LookUpVar(Symbol sym) {
    for (int idx = m_var_idx_tab.size() - 1; idx >= 0; --idx) {
      if (m_var_idx_tab[idx] == sym) {
        return m_var_idx_tab.size() - 1 - idx;
      }
    }
    return -1;
  }

  int AddVar(Symbol sym) {
    m_var_idx_tab.push_back(sym);
    ++m_scope_lengths[m_scope_lengths.size() - 1];
    return m_var_idx_tab.size() - 1;
  }

  int LookUpParam(Symbol sym) {
    for (int idx = 0; idx < m_param_idx_tab.size(); ++idx) {
      if (m_param_idx_tab[idx] == sym) {
        return m_param_idx_tab.size() - 1 - idx;
      }
    }
    return -1;
  }

  int AddParam(Symbol sym) {
    m_param_idx_tab.push_back(sym);
    return m_param_idx_tab.size() - 1;
  }

  int AddObstacle();

  std::vector<int> m_scope_lengths;
  std::vector<Symbol> m_var_idx_tab;
  std::vector<Symbol> m_param_idx_tab;
  CgenNode* m_class_node;
};