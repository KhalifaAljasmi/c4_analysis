/*
    This is a compact C compiler capable of compiling itself.
    It includes a tokenizer, parser, and a virtual machine for code execution.

    The compiler consists of three main functions:
    - next(): Handles tokenization.
    - expr(): Parses expressions.
    - stmt(): Parses statements.
*/

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <unistd.h>
#include <fcntl.h>
#define int long long

char *p, *lp, // current position in source code
     *data;   // data/bss pointer

int *e, *le,  // current position in emitted code
    *id,      // currently parsed identifier
    *sym,     // symbol table (simple list of identifiers)
    tk,       // current token
    ival,     // current token value
    ty,       // current expression type
    loc,      // local variable offset
    line,     // current line number
    src,      // print source and assembly flag
    debug;    // print executed instructions

// tokens and classes (operators last and in precedence order)
enum {
  Num = 128, Fun, Sys, Glo, Loc, Id,
  Char, Else, Enum, If, Int, Return, Sizeof, While,
  Assign, Cond, Lor, Lan, Or, Xor, And, Eq, Ne, Lt, Gt, Le, Ge, Shl, Shr, Add, Sub, Mul, Div, Mod, Inc, Dec, Brak
};

// opcodes
enum { LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,
       OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,
       OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT };

// types
enum { CHAR, INT, PTR };

// identifier offsets (since we can't create an ident struct)
enum { Tk, Hash, Name, Class, Type, Val, HClass, HType, HVal, Idsz };

void next()
{
  char *pp; // Temporary pointer to track the start of identifiers

  while (tk = *p) { // Read the current character as a token
    ++p; // Move to the next character
    if (tk == '\n') { // Handle newlines
      if (src) { // If source debugging is enabled
        printf("%d: %.*s", line, p - lp, lp); // Print the current line
        lp = p; // Update the start of the next line
        while (le < e) { // Print emitted instructions
          printf("%8.4s", &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
                           "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
                           "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[*++le * 5]); // Print instruction names
          if (*le <= ADJ) printf(" %d\n", *++le); else printf("\n"); // Print instruction operands
        }
      }
      ++line; // Increment the line counter
    }
    else if (tk == '#') { // Handle preprocessor directives
      while (*p != 0 && *p != '\n') ++p; // Skip until the end of the line
    }
    else if ((tk >= 'a' && tk <= 'z') || (tk >= 'A' && tk <= 'Z') || tk == '_') { // Handle identifiers
      pp = p - 1; // Track the start of the identifier
      while ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') || (*p >= '0' && *p <= '9') || *p == '_') // Read the full identifier
        tk = tk * 147 + *p++; // Generate a hash for the identifier
      tk = (tk << 6) + (p - pp); // Combine hash with length
      id = sym; // Start searching the symbol table
      while (id[Tk]) { // Check for existing identifiers
        if (tk == id[Hash] && !memcmp((char *)id[Name], pp, p - pp)) { tk = id[Tk]; return; } // Return if identifier exists
        id = id + Idsz;
      }
      id[Name] = (int)pp; // Store new identifier in the symbol table
      id[Hash] = tk; // Save the hash
      tk = id[Tk] = Id; // Mark as an identifier
      return;
    }
    else if (tk >= '0' && tk <= '9') { // Handle numbers
      if (ival = tk - '0') { while (*p >= '0' && *p <= '9') ival = ival * 10 + *p++ - '0'; } // Decimal numbers
      else if (*p == 'x' || *p == 'X') { // Hexadecimal numbers
        while ((tk = *++p) && ((tk >= '0' && tk <= '9') || (tk >= 'a' && tk <= 'f') || (tk >= 'A' && tk <= 'F'))) // Read hex digits
          ival = ival * 16 + (tk & 15) + (tk >= 'A' ? 9 : 0); // Convert to integer
      }
      else { while (*p >= '0' && *p <= '7') ival = ival * 8 + *p++ - '0'; } // Octal numbers
      tk = Num; // Mark as a number
      return;
    }
    else if (tk == '/') { // Handle comments and division
      if (*p == '/') { // Single-line comments
        ++p;
        while (*p != 0 && *p != '\n') ++p; // Skip the comment
      }
      else {
        tk = Div; // Division operator
        return;
      }
    }
    else if (tk == '\'' || tk == '"') { // Handle character and string literals
      pp = data; // Track the start of the string
      while (*p != 0 && *p != tk) { // Read until the closing quote
        if ((ival = *p++) == '\\') { // Handle escape sequences
          if ((ival = *p++) == 'n') ival = '\n'; // Newline escape
        }
        if (tk == '"') *data++ = ival; // Store string data
      }
      ++p;
      if (tk == '"') ival = (int)pp; else tk = Num; // Store address or character value
      return;
    }
    // Handle operators and punctuation
    else if (tk == '=') { if (*p == '=') { ++p; tk = Eq; } else tk = Assign; return; }
    else if (tk == '+') { if (*p == '+') { ++p; tk = Inc; } else tk = Add; return; }
    else if (tk == '-') { if (*p == '-') { ++p; tk = Dec; } else tk = Sub; return; }
    else if (tk == '!') { if (*p == '=') { ++p; tk = Ne; } return; }
    else if (tk == '<') { if (*p == '=') { ++p; tk = Le; } else if (*p == '<') { ++p; tk = Shl; } else tk = Lt; return; }
    else if (tk == '>') { if (*p == '=') { ++p; tk = Ge; } else if (*p == '>') { ++p; tk = Shr; } else tk = Gt; return; }
    else if (tk == '|') { if (*p == '|') { ++p; tk = Lor; } else tk = Or; return; }
    else if (tk == '&') { if (*p == '&') { ++p; tk = Lan; } else tk = And; return; }
    else if (tk == '^') { tk = Xor; return; }
    else if (tk == '%') { tk = Mod; return; }
    else if (tk == '*') { tk = Mul; return; }
    else if (tk == '[') { tk = Brak; return; }
    else if (tk == '?') { tk = Cond; return; }
    else if (tk == '~' || tk == ';' || tk == '{' || tk == '}' || tk == '(' || tk == ')' || tk == ']' || tk == ',' || tk == ':') return;
  }
}

void expr(int lev)
{
  int t, *d; // Temporary variables for types and jump targets

  if (!tk) { printf("%d: unexpected eof in expression\n", line); exit(-1); } // Handle unexpected end of input
  else if (tk == Num) { *++e = IMM; *++e = ival; next(); ty = INT; } // Handle numeric literals
  else if (tk == '"') { // Handle string literals
    *++e = IMM; *++e = ival; next(); // Emit instruction for string address
    while (tk == '"') next(); // Skip repeated quotes
    data = (char *)((int)data + sizeof(int) & -sizeof(int)); ty = PTR; // Align memory for strings
  }
  else if (tk == Sizeof) { // Handle sizeof operator
    next();
    if (tk == '(') next(); else { printf("%d: open paren expected in sizeof\n", line); exit(-1); }
    ty = INT;
    if (tk == Int) next(); // Handle sizeof(int)
    else if (tk == Char) { next(); ty = CHAR; } // Handle sizeof(char)
    while (tk == Mul) { next(); ty = ty + PTR; } // Handle pointer types
    if (tk == ')') next(); else { printf("%d: close paren expected in sizeof\n", line); exit(-1); }
    *++e = IMM; *++e = (ty == CHAR) ? sizeof(char) : sizeof(int); // Emit size value
    ty = INT;
  }
  else if (tk == Id) { // Handle identifiers
    d = id; next(); // Store identifier info
    if (tk == '(') { // Handle function calls
      next();
      t = 0; // Track argument count
      while (tk != ')'){ expr(Assign); *++e = PSH; ++t; if (tk == ',') next(); } // Process arguments
      next();
      if (d[Class] == Sys) *++e = d[Val]; // Handle system functions
      else if (d[Class] == Fun) { *++e = JSR; *++e = d[Val]; } // Handle user-defined functions
      else { printf("%d: bad function call\n", line); exit(-1); }
      if (t) { *++e = ADJ; *++e = t; } // Adjust stack for arguments
      ty = d[Type]; // Set return type
    }
    else if (d[Class] == Num) { *++e = IMM; *++e = d[Val]; ty = INT; } // Handle constants
    else { // Handle variables
      if (d[Class] == Loc) { *++e = LEA; *++e = loc - d[Val]; } // Local variables
      else if (d[Class] == Glo) { *++e = IMM; *++e = d[Val]; } // Global variables
      else { printf("%d: undefined variable\n", line); exit(-1); }
      *++e = ((ty = d[Type]) == CHAR) ? LC : LI; // Load variable value
    }
  }
  else if (tk == '(') { // Handle parentheses
    next();
    if (tk == Int || tk == Char) { // Handle typecasting
      t = (tk == Int) ? INT : CHAR; next();
      while (tk == Mul) { next(); t = t + PTR; }
      if (tk == ')') next(); else { printf("%d: bad cast\n", line); exit(-1); }
      expr(Inc); // Parse the expression being cast
      ty = t; // Set the type
    }
    else { // Handle normal expressions
      expr(Assign); // Parse the expression
      if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    }
  }
  else if (tk == Mul) { // Handle dereferencing
    next(); expr(Inc);
    if (ty > INT) ty = ty - PTR; else { printf("%d: bad dereference\n", line); exit(-1); }
    *++e = (ty == CHAR) ? LC : LI; // Load value from pointer
  }
  else if (tk == And) { // Handle address-of operator
    next(); expr(Inc);
    if (*e == LC || *e == LI) --e; else { printf("%d: bad address-of\n", line); exit(-1); }
    ty = ty + PTR; // Increment pointer level
  }
  else if (tk == '!') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = 0; *++e = EQ; ty = INT; } // Logical NOT
  else if (tk == '~') { next(); expr(Inc); *++e = PSH; *++e = IMM; *++e = -1; *++e = XOR; ty = INT; } // Bitwise NOT
  else if (tk == Add) { next(); expr(Inc); ty = INT; } // Unary plus (ignored)
  else if (tk == Sub) { // Unary minus
    next(); *++e = IMM;
    if (tk == Num) { *++e = -ival; next(); } else { *++e = -1; *++e = PSH; expr(Inc); *++e = MUL; }
    ty = INT;
  }
  else if (tk == Inc || tk == Dec) { // Handle increment/decrement
    t = tk; next(); expr(Inc);
    if (*e == LC) { *e = PSH; *++e = LC; }
    else if (*e == LI) { *e = PSH; *++e = LI; }
    else { printf("%d: bad lvalue in pre-increment\n", line); exit(-1); }
    *++e = PSH;
    *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
    *++e = (t == Inc) ? ADD : SUB;
    *++e = (ty == CHAR) ? SC : SI;
  }
  else { printf("%d: bad expression\n", line); exit(-1); } // Handle invalid expressions

  while (tk >= lev) { // Handle operator precedence
    t = ty;
    if (tk == Assign) { // Handle assignment
      next();
      if (*e == LC || *e == LI) *e = PSH; else { printf("%d: bad lvalue in assignment\n", line); exit(-1); }
      expr(Assign); *++e = ((ty = t) == CHAR) ? SC : SI;
    }
    else if (tk == Cond) { // Handle ternary operator
      next();
      *++e = BZ; d = ++e;
      expr(Assign);
      if (tk == ':') next(); else { printf("%d: conditional missing colon\n", line); exit(-1); }
      *d = (int)(e + 3); *++e = JMP; d = ++e;
      expr(Cond);
      *d = (int)(e + 1);
    }
    else if (tk == Lor) { next(); *++e = BNZ; d = ++e; expr(Lan); *d = (int)(e + 1); ty = INT; }
    else if (tk == Lan) { next(); *++e = BZ;  d = ++e; expr(Or);  *d = (int)(e + 1); ty = INT; }
    else if (tk == Or)  { next(); *++e = PSH; expr(Xor); *++e = OR;  ty = INT; }
    else if (tk == Xor) { next(); *++e = PSH; expr(And); *++e = XOR; ty = INT; }
    else if (tk == And) { next(); *++e = PSH; expr(Eq);  *++e = AND; ty = INT; }
    else if (tk == Eq)  { next(); *++e = PSH; expr(Lt);  *++e = EQ;  ty = INT; }
    else if (tk == Ne)  { next(); *++e = PSH; expr(Lt);  *++e = NE;  ty = INT; }
    else if (tk == Lt)  { next(); *++e = PSH; expr(Shl); *++e = LT;  ty = INT; }
    else if (tk == Gt)  { next(); *++e = PSH; expr(Shl); *++e = GT;  ty = INT; }
    else if (tk == Le)  { next(); *++e = PSH; expr(Shl); *++e = LE;  ty = INT; }
    else if (tk == Ge)  { next(); *++e = PSH; expr(Shl); *++e = GE;  ty = INT; }
    else if (tk == Shl) { next(); *++e = PSH; expr(Add); *++e = SHL; ty = INT; }
    else if (tk == Shr) { next(); *++e = PSH; expr(Add); *++e = SHR; ty = INT; }
    else if (tk == Add) {
      next(); *++e = PSH; expr(Mul);
      if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }
      *++e = ADD;
    }
    else if (tk == Sub) {
      next(); *++e = PSH; expr(Mul);
      if (t > PTR && t == ty) { *++e = SUB; *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = DIV; ty = INT; }
      else if ((ty = t) > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL; *++e = SUB; }
      else *++e = SUB;
    }
    else if (tk == Mul) { next(); *++e = PSH; expr(Inc); *++e = MUL; ty = INT; }
    else if (tk == Div) { next(); *++e = PSH; expr(Inc); *++e = DIV; ty = INT; }
    else if (tk == Mod) { next(); *++e = PSH; expr(Inc); *++e = MOD; ty = INT; }
    else if (tk == Inc || tk == Dec) {
      if (*e == LC) { *e = PSH; *++e = LC; }
      else if (*e == LI) { *e = PSH; *++e = LI; }
      else { printf("%d: bad lvalue in post-increment\n", line); exit(-1); }
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
      *++e = (tk == Inc) ? ADD : SUB;
      *++e = (ty == CHAR) ? SC : SI;
      *++e = PSH; *++e = IMM; *++e = (ty > PTR) ? sizeof(int) : sizeof(char);
      *++e = (tk == Inc) ? SUB : ADD;
      next();
    }
    else if (tk == Brak) {
      next(); *++e = PSH; expr(Assign);
      if (tk == ']') next(); else { printf("%d: close bracket expected\n", line); exit(-1); }
      if (t > PTR) { *++e = PSH; *++e = IMM; *++e = sizeof(int); *++e = MUL;  }
      else if (t < PTR) { printf("%d: pointer type expected\n", line); exit(-1); }
      *++e = ADD;
      *++e = ((ty = t - PTR) == CHAR) ? LC : LI;
    }
    else { printf("%d: compiler error tk=%d\n", line, tk); exit(-1); }
  }
}
void stmt()
{
  int *a, *b; // Temporary pointers for branch targets

  if (tk == If) { // Handle if statements
    next();
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign); // Evaluate condition
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    *++e = BZ; b = ++e; // Emit branch if false
    stmt(); // Parse if body
    if (tk == Else) { // Handle else
      *b = (int)(e + 3); *++e = JMP; b = ++e; // Patch if branch and jump to else
      next();
      stmt(); // Parse else body
    }
    *b = (int)(e + 1); // Patch the jump target
  }
  else if (tk == While) { // Handle while loops
    next();
    a = e + 1; // Store loop start
    if (tk == '(') next(); else { printf("%d: open paren expected\n", line); exit(-1); }
    expr(Assign); // Evaluate condition
    if (tk == ')') next(); else { printf("%d: close paren expected\n", line); exit(-1); }
    *++e = BZ; b = ++e; // Emit branch if false
    stmt(); // Parse loop body
    *++e = JMP; *++e = (int)a; // Jump back to loop start
    *b = (int)(e + 1); // Patch exit branch
  }
  else if (tk == Return) { // Handle return statements
    next();
    if (tk != ';') expr(Assign); // Evaluate return value
    *++e = LEV; // Emit return instruction
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
  else if (tk == '{') { // Handle blocks
    next();
    while (tk != '}') stmt(); // Parse statements in the block
    next();
  }
  else if (tk == ';') { // Handle empty statements
    next();
  }
  else { // Handle expressions
    expr(Assign);
    if (tk == ';') next(); else { printf("%d: semicolon expected\n", line); exit(-1); }
  }
}

int main(int argc, char **argv)
{
  int fd, bt, ty, poolsz, *idmain;
  int *pc, *sp, *bp, a, cycle; // VM registers
  int i, *t; // Temporary variables

  --argc; ++argv; // Skip program name
  if (argc > 0 && **argv == '-' && (*argv)[1] == 's') { src = 1; --argc; ++argv; } // Enable source debugging
  if (argc > 0 && **argv == '-' && (*argv)[1] == 'd') { debug = 1; --argc; ++argv; } // Enable execution debugging
  if (argc < 1) { printf("usage: c4 [-s] [-d] file ...\n"); return -1; } // Print usage if no input file

  if ((fd = open(*argv, 0)) < 0) { printf("could not open(%s)\n", *argv); return -1; } // Open source file

  poolsz = 256*1024; // Allocate memory pools
  if (!(sym = malloc(poolsz))) { printf("could not malloc(%d) symbol area\n", poolsz); return -1; }
  if (!(le = e = malloc(poolsz))) { printf("could not malloc(%d) text area\n", poolsz); return -1; }
  if (!(data = malloc(poolsz))) { printf("could not malloc(%d) data area\n", poolsz); return -1; }
  if (!(sp = malloc(poolsz))) { printf("could not malloc(%d) stack area\n", poolsz); return -1; }

  memset(sym,  0, poolsz); // Initialize memory
  memset(e,    0, poolsz);
  memset(data, 0, poolsz);

  // Define keywords and standard library functions
  p = "char else enum if int return sizeof while "
      "open read close printf malloc free memset memcmp exit void main";
  i = Char; while (i <= While) { next(); id[Tk] = i++; } // Add keywords to symbol table
  i = OPEN; while (i <= EXIT) { next(); id[Class] = Sys; id[Type] = INT; id[Val] = i++; } // Add library functions
  next(); id[Tk] = Char; // Handle void type
  next(); idmain = id; // Track main function

  if (!(lp = p = malloc(poolsz))) { printf("could not malloc(%d) source area\n", poolsz); return -1; } // Allocate source buffer
  if ((i = read(fd, p, poolsz-1)) <= 0) { printf("read() returned %d\n", i); return -1; } // Read source file
  p[i] = 0; // Null-terminate source
  close(fd); // Close file

  // Parse declarations
  line = 1;
  next();
  while (tk) {
    bt = INT; // Base type
    if (tk == Int) next();
    else if (tk == Char) { next(); bt = CHAR; }
    else if (tk == Enum) { // Handle enums
      next();
      if (tk != '{') next();
      if (tk == '{') {
        next();
        i = 0;
        while (tk != '}') { // Process enum values
          if (tk != Id) { printf("%d: bad enum identifier %d\n", line, tk); return -1; }
          next();
          if (tk == Assign) {
            next();
            if (tk != Num) { printf("%d: bad enum initializer\n", line); return -1; }
            i = ival;
            next();
          }
          id[Class] = Num; id[Type] = INT; id[Val] = i++;
          if (tk == ',') next();
        }
        next();
      }
    }
    while (tk != ';' && tk != '}') { // Handle global variables and functions
      ty = bt;
      while (tk == Mul) { next(); ty = ty + PTR; } // Handle pointers
      if (tk != Id) { printf("%d: bad global declaration\n", line); return -1; }
      if (id[Class]) { printf("%d: duplicate global definition\n", line); return -1; }
      next();
      id[Type] = ty;
      if (tk == '(') { // Handle function declarations
        id[Class] = Fun;
        id[Val] = (int)(e + 1);
        next(); i = 0;
        while (tk != ')') { // Parse function parameters
          ty = INT;
          if (tk == Int) next();
          else if (tk == Char) { next(); ty = CHAR; }
          while (tk == Mul) { next(); ty = ty + PTR; }
          if (tk != Id) { printf("%d: bad parameter declaration\n", line); return -1; }
          if (id[Class] == Loc) { printf("%d: duplicate parameter definition\n", line); return -1; }
          id[HClass] = id[Class]; id[Class] = Loc;
          id[HType]  = id[Type];  id[Type] = ty;
          id[HVal]   = id[Val];   id[Val] = i++;
          next();
          if (tk == ',') next();
        }
        next();
        if (tk != '{') { printf("%d: bad function definition\n", line); return -1; }
        loc = ++i;
        next();
        while (tk == Int || tk == Char) { // Parse local variables
          bt = (tk == Int) ? INT : CHAR;
          next();
          while (tk != ';') {
            ty = bt;
            while (tk == Mul) { next(); ty = ty + PTR; }
            if (tk != Id) { printf("%d: bad local declaration\n", line); return -1; }
            if (id[Class] == Loc) { printf("%d: duplicate local definition\n", line); return -1; }
            id[HClass] = id[Class]; id[Class] = Loc;
            id[HType]  = id[Type];  id[Type] = ty;
            id[HVal]   = id[Val];   id[Val] = ++i;
            next();
            if (tk == ',') next();
          }
          next();
        }
        *++e = ENT; *++e = i - loc; // Emit function prologue
        while (tk != '}') stmt(); // Parse function body
        *++e = LEV; // Emit function epilogue
        id = sym; // Unwind symbol table
        while (id[Tk]) {
          if (id[Class] == Loc) {
            id[Class] = id[HClass];
            id[Type] = id[HType];
            id[Val] = id[HVal];
          }
          id = id + Idsz;
        }
      }
      else { // Handle global variables
        id[Class] = Glo;
        id[Val] = (int)data;
        data = data + sizeof(int);
      }
      if (tk == ',') next();
    }
    next();
  }

  if (!(pc = (int *)idmain[Val])) { printf("main() not defined\n"); return -1; } // Ensure main is defined
  if (src) return 0; // Exit if source debugging is enabled

  // Setup stack
  bp = sp = (int *)((int)sp + poolsz);
  *--sp = EXIT; // Call exit if main returns
  *--sp = PSH; t = sp;
  *--sp = argc;
  *--sp = (int)argv;
  *--sp = (int)t;

  // Execute compiled program
  cycle = 0;
  while (1) {
    i = *pc++; ++cycle;
    if (debug) { // Print debug info
      printf("%d> %.4s", cycle,
        &"LEA ,IMM ,JMP ,JSR ,BZ  ,BNZ ,ENT ,ADJ ,LEV ,LI  ,LC  ,SI  ,SC  ,PSH ,"
         "OR  ,XOR ,AND ,EQ  ,NE  ,LT  ,GT  ,LE  ,GE  ,SHL ,SHR ,ADD ,SUB ,MUL ,DIV ,MOD ,"
         "OPEN,READ,CLOS,PRTF,MALC,FREE,MSET,MCMP,EXIT,"[i * 5]);
      if (i <= ADJ) printf(" %d\n", *pc); else printf("\n");
    }
    if      (i == LEA) a = (int)(bp + *pc++);                             // Load local address
    else if (i == IMM) a = *pc++;                                         // Load global address or immediate
    else if (i == JMP) pc = (int *)*pc;                                   // Jump
    else if (i == JSR) { *--sp = (int)(pc + 1); pc = (int *)*pc; }        // Jump to subroutine
    else if (i == BZ)  pc = a ? pc + 1 : (int *)*pc;                      // Branch if zero
    else if (i == BNZ) pc = a ? (int *)*pc : pc + 1;                      // Branch if not zero
    else if (i == ENT) { *--sp = (int)bp; bp = sp; sp = sp - *pc++; }     // Enter subroutine
    else if (i == ADJ) sp = sp + *pc++;                                   // Adjust stack
    else if (i == LEV) { sp = bp; bp = (int *)*sp++; pc = (int *)*sp++; } // Leave subroutine
    else if (i == LI)  a = *(int *)a;                                     // Load int
    else if (i == LC)  a = *(char *)a;                                    // Load char
    else if (i == SI)  *(int *)*sp++ = a;                                 // Store int
    else if (i == SC)  a = *(char *)*sp++ = a;                            // Store char
    else if (i == PSH) *--sp = a;                                         // Push

    else if (i == OR)  a = *sp++ |  a;
    else if (i == XOR) a = *sp++ ^  a;
    else if (i == AND) a = *sp++ &  a;
    else if (i == EQ)  a = *sp++ == a;
    else if (i == NE)  a = *sp++ != a;
    else if (i == LT)  a = *sp++ <  a;
    else if (i == GT)  a = *sp++ >  a;
    else if (i == LE)  a = *sp++ <= a;
    else if (i == GE)  a = *sp++ >= a;
    else if (i == SHL) a = *sp++ << a;
    else if (i == SHR) a = *sp++ >> a;
    else if (i == ADD) a = *sp++ +  a;
    else if (i == SUB) a = *sp++ -  a;
    else if (i == MUL) a = *sp++ *  a;
    else if (i == DIV) a = *sp++ /  a;
    else if (i == MOD) a = *sp++ %  a;

    else if (i == OPEN) a = open((char *)sp[1], *sp);
    else if (i == READ) a = read(sp[2], (char *)sp[1], *sp);
    else if (i == CLOS) a = close(*sp);
    else if (i == PRTF) { t = sp + pc[1]; a = printf((char *)t[-1], t[-2], t[-3], t[-4], t[-5], t[-6]); }
    else if (i == MALC) a = (int)malloc(*sp);
    else if (i == FREE) free((void *)*sp);
    else if (i == MSET) a = (int)memset((char *)sp[2], sp[1], *sp);
    else if (i == MCMP) a = memcmp((char *)sp[2], (char *)sp[1], *sp);
    else if (i == EXIT) { printf("exit(%d) cycle = %d\n", *sp, cycle); return *sp; }
    else { printf("unknown instruction = %d! cycle = %d\n", i, cycle); return -1; }
  }
}
