%start Program
%parse-param tree: &crate::tree::TreeBuilder<AstNode> 
%%
Program -> Result<AstRef>: 
    Trivia DeclList { node!(tree, $span, Program { definitions: $2? }) }
  ;

DeclList -> Result<Vec<DefaultLinearTreeId>>:
    Decl { Ok(vec![$1?]) }
  | DeclList Decl { 
      let mut arr = $1?;
      arr.push($2?);
      Ok(arr)
    }
  ;

/// BEGIN: Top-Level Declarations
// Howlite supports the following top-level declarations:
//  - "type"
//  - "func"
//  - "use"
//  - "extern"

Decl -> Result<DefaultLinearTreeId>:
    DeclTy { $1 }
  | DefFunc { $1 }
  | DefExternFunc { $1 }
  | DefExternVar { $1 }
  | DefImport { $1 }
  | DefGlobal { $1 }
  ;

DefImport -> Result<DefaultLinearTreeId>:
    'use' TriviaRequired 'STRING' Trivia ';' Trivia {
      trivia!(right trivia_tree, $6,
        node!(tree, $span, DefImport {
          file: $lexer.span_str($3?.span()).into(),
        }))
    }
  ;

DefGlobal -> Result<DefaultLinearTreeId>:
    ExprLet ';' Trivia {
      $1 // TODO: inner trivia
    }
  ;

DeclTy -> Result<DefaultLinearTreeId>:
    'type' TriviaRequired 'IDENT' Trivia '[' Trivia TyParamDeclList ']' Trivia '=' Trivia TyExpr ';' Trivia {
      // TODO: inner trivia
      trivia!(right trivia_tree, $14,
        node!(tree, $span, DefType {
            name: $lexer.span_str($3?.span()).into(),
            alias: false,
            ty: trivia!(left trivia_tree, $11, $12?),
            ty_params: $7?,
        }))
    }
  | 'type' TriviaRequired 'alias' TriviaRequired 'IDENT' Trivia '[' Trivia TyParamDeclList ']' Trivia '=' Trivia TyExpr ';' Trivia {
      // TODO: inner trivia
      trivia!(right trivia_tree, $15,
        node!(tree, $span, DefType {
            name: $lexer.span_str($5?.span()).into(),
            alias: true,
            ty: trivia!(left trivia_tree, $13, $14?),
            ty_params: $9?,
        }))
    }
  | 'type' TriviaRequired 'IDENT' Trivia '=' Trivia TyExpr ';' Trivia {
      // TODO: inner trivia
      trivia!(right trivia_tree, $10,
        node!(tree, $span, DefType {
            name: $lexer.span_str($3?.span()).into(),
            alias: false,
            ty: trivia!(left trivia_tree, $6, $7?),
            ty_params: vec![],
        }))
    }
  | 'type' TriviaRequired 'alias' TriviaRequired IDENT Trivia '=' Trivia TyExpr ';' Trivia {
      // TODO: inner trivia
      trivia!(right trivia_tree, $11,
        node!(tree, $span, DefType {
            name: $lexer.span_str($5?.span()).into(),
            alias: true,
            ty: trivia!(left trivia_tree, $8, $9?),
            ty_params: vec![],
        }))
    }
  ;

TyParamDeclList -> Result<Vec<DefaultLinearTreeId>>:
    TyParamDecl { Ok(vec![$1?]) }
  | TyParamDeclList ',' Trivia TyParamDecl {
      let mut arr = $1?;
      arr.push(trivia!(left trivia_tree, $3, $4?));
      Ok(arr)
    }
  ;

TyParamDecl -> Result<DefaultLinearTreeId>:
    IDENT Trivia ':' Trivia TyExpr {
      node!(tree, $span,
        TyParam {
          name: $lexer.span_str($1?.span()).into(),
          super_ty: trivia!(left trivia_tree, $4, $5?),
          default_ty: None
        }
      )
    }
  ;

DefFunc -> Result<DefaultLinearTreeId>:
    // TODO: (both productions) inner trivia
    'func' TriviaRequired IDENT Trivia '(' Trivia DefFuncParamList ')' Trivia ':' Trivia TyExpr ExprBlock {
      node!(tree, $span, DefFunc {
        name: $lexer.span_str($3?.span()).into(),
        params: $7?,
        ty_params: vec![],
        return_ty: trivia!(left trivia_tree, $11, $12?),
        body: $13?
      })
    }
  | 'func' TriviaRequired IDENT Trivia '[' Trivia TyParamDeclList ']' '(' Trivia DefFuncParamList ')' Trivia ':' Trivia TyExpr ExprBlock {
      node!(tree, $span, DefFunc {
        name: $lexer.span_str($3?.span()).into(),
        params: $11?,
        ty_params: $7?, // TODO: lhs trivia
        return_ty: trivia!(left trivia_tree, $15, $16?),
        body: $17?
      })
    }
  ;

DefExternFunc -> Result<DefaultLinearTreeId>:
    // TODO: outer & inner trivia
    'extern' TriviaRequired 'func' TriviaRequired IDENT Trivia '(' Trivia DefFuncParamList ')' Trivia ':' Trivia TyExpr ';' Trivia {
      trivia!(left trivia_tree, $16,
        node!(tree, $span, DefExternFunc {
          name: $lexer.span_str($5?.span()).into(),
          params: $9?,
          ty_params: vec![],
          return_ty: trivia!(left trivia_tree, $13, $14?),
        }))
    }
  | 'extern' TriviaRequired 'func' TriviaRequired IDENT Trivia '[' Trivia TyParamDeclList ']' '(' Trivia DefFuncParamList ')' Trivia ':' Trivia TyExpr ';' Trivia {
      trivia!(left trivia_tree, $20,
        node!(tree, $span, DefExternFunc {
          name: $lexer.span_str($5?.span()).into(),
          params: $13?,
          ty_params: $9?,
          return_ty: trivia!(left trivia_tree, $17, $18?),
        }))
    }
  ;

DefExternVar -> Result<DefaultLinearTreeId>:
    'extern' TriviaRequired 'let' TriviaRequired IDENT Trivia ':' Trivia TyExpr ';' Trivia {
      // TODO: inner trivia
      trivia!(left trivia_tree, $11,
        node!(tree, $span, DefExternVar {
          name: $lexer.span_str($5?.span()).into(),
          ty: trivia!(left trivia_tree, $8, $9?),
          mutable: false,
        }))
    }
  | 'extern' TriviaRequired 'let' TriviaRequired 'mut' TriviaRequired IDENT Trivia ':' Trivia TyExpr ';' Trivia {
      // TODO: inner trivia
      trivia!(left trivia_tree, $13,
        node!(tree, $span, DefExternVar {
          name: $lexer.span_str($7?.span()).into(),
          ty: trivia!(left trivia_tree, $10, $11?),
          mutable: true,
        }))
    }
  ;


DefFuncParamList -> Result<Vec<AstRef>>:
    DefFuncParam { Ok(vec![$1?]) }
  | DefFuncParamList ',' Trivia DefFuncParam {
      let mut arr = $1?;
      arr.push(trivia!(right trivia_tree, $4, trivia!(left trivia_tree, $3, $4?)));
      Ok(arr)
    }
  | %empty { Ok(vec![]) }
  ;

DefFuncParam -> Result<AstRef>:
    'mut' IDENT Trivia ':' Trivia TyExpr {
      node!(tree, $span, DefParam {
        mutable: true,
        ty: trivia!(left trivia_tree, $5, $6?),
        name: $lexer.span_str($1?.span()).into()
      })
    }
  | IDENT Trivia ':' Trivia TyExpr {
      node!(tree, $span, DefParam {
        mutable: false,
        ty: trivia!(left trivia_tree, $4, $5?),
        name: $lexer.span_str($1?.span()).into()
      })
    }
  ;



/// END: Top-Level Declarations

ExprTypeConstruction -> Result<AstRef>:
    Term ':' Trivia TyTerm {
      node!(tree, $span,
        ExprTypeConstruction { 
          ty: trivia!(left trivia_tree, $3, $4?),
          value: $1?
        })
    }
  ;

/// BEGIN: Full Expressions

Expr -> Result<AstRef>:
    ExprInfix { $1 }
  | ExprLet { $1 }
  | ExprIf { $1 }
  | ExprBlock { $1 }
  | ExprWhile { $1 }
  | ExprTypeConstruction { $1 }
  ;

/// END: Full Expression


/// BEGIN: Block Expression

ExprBlock -> Result<AstRef>:
    '{' Trivia ExprBlockStmtList ';' Trivia '}' Trivia {
      // TODO: which node do we associate the inner trivia with?
      trivia!(left trivia_tree, $7,
        node!(tree, $span, Block {
          returns: false,
          statements: $3?
        }))
    }
  | '{' Trivia ExprBlockStmtList '}' Trivia {
    trivia!(left trivia_tree, $7,
        node!(tree, $span, Block {
          returns: true,
          statements: $3?
        }))
    }
  ;

ExprBlockStmtList -> Result<Vec<AstRef>>:
    ExprBlockStmtList ';' Trivia Expr {
      let mut arr = $1?;
      arr.push(trivia!(left trivia_tree, $3, $4?));
      Ok(arr)
    }
  | Expr { Ok(vec![$1?]) }
  ;

/// BEGIN: Let Expression
ExprLet -> Result<AstRef>:
    // TODO: (both productions) inner trivia
    'let' TriviaRequired 'IDENT' Trivia ':' Trivia TyExpr '=' Trivia ExprSimple { 
      node!(tree, $span, ExprLet {
        name:  $lexer.span_str($3?.span()).into(),
        ty: trivia!(left trivia_tree, $6, $7?),
        value: trivia!(left trivia_tree, $9, $10?),
        mutable: false,
      })
    }
  | 'let' TriviaRequired 'mut' TriviaRequired 'IDENT' TriviaRequired ':' Trivia TyExpr '=' Trivia ExprSimple { 
      node!(tree, $span, ExprLet {
        name: $lexer.span_str($5?.span()).into(),
        ty: trivia!(left trivia_tree, $8, $9?),
        value: trivia!(left trivia_tree, $11, $12?),
        mutable: true,
      })
    }
  ;

// Like Expr, but force keyword and assign expressions to be in parens.
ExprSimple -> Result<AstRef>: ExprInfixLogic { $1 };

/// END: Let Expression

/// BEGIN: If Expression

ExprIf -> Result<AstRef>:
    'if' TriviaRequired Expr ExprBlock ExprElse {
      node!(tree, $span, ExprIf {
        condition: trivia!(left trivia_tree, $2, $3?),
        success: $4?,
        failure: Some($5?)
      })
    }
  | 'if' TriviaRequired Expr ExprBlock {
      node!(tree, $span, ExprIf {
        condition: trivia!(left trivia_tree, $2, $3?),
        success: $4?,
        failure: None,
      })
    }
  ;

ExprElse -> Result<AstRef>:
    'else' Trivia ExprBlock { 
      trivia!(left trivia_tree, $2, $3)
    }
  | 'else' TriviaRequired ExprIf {
      trivia!(left trivia_tree, $2, $3)
    }
  ;

/// END: If Expression


/// BEGIN: While Expression

ExprWhile -> Result<AstRef>:
    'while' TriviaRequired Expr ExprBlock {
      node!(tree, $span, ExprWhile {
        condition: trivia!(left trivia_tree, $2, $3?),
        body: $4?,
      })
    }
  ;

/// END: While Expression



/// BEGIN: Call Expression

ExprCall -> Result<AstRef>:
    Term '(' Trivia ExprCallParamListOpt ')' Trivia {
      // TODO: inner trivia
      trivia!(right trivia_tree, $6,
        node!(tree, $span, ExprCall {
          callee: $1?,
          params: $4?,
          ty_params: vec![]
        }))
    }
  | Term ':[' Trivia TyParamList ']' Trivia '(' Trivia ExprCallParamListOpt ')' Trivia {
      // TODO: inner trivia
      trivia!(right trivia_tree, $11,
        node!(tree, $span, ExprCall {
          callee: $1?,
          params: $9?,
          ty_params: $4?, // TODO: inner trivia
        }))
    }
  ;

ExprCallParamListOpt -> Result<Vec<AstRef>>:
    ExprCallParamList { $1 }
  | %empty { Ok(vec![]) }
  ;

ExprCallParamList -> Result<Vec<AstRef>>:
  // TODO: allow trailing comma.
    Expr { Ok(vec![$1?]) }
  | ExprCallParamList ',' Trivia Expr {
      let mut arr = $1?;
      arr.push(trivia!(left trivia_tree, $3, $4?));
      Ok(arr)
    }
  ;

/// END: Call Expression


/// BEGIN: Infix Expressions

ExprInfix -> Result<AstRef>:
  ExprInfixAssign { $1 }
  ;

ExprInfixAssign -> Result<AstRef>:
   ExprInfixLogic '=' Trivia ExprInfixLogic { infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::Assign) }
  | ExprInfixLogic { $1 }
  ;

ExprInfixLogic -> Result<AstRef>:
    ExprInfixLogic '&&' Trivia ExprInfixCompare {
      infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::LogicalAnd)
  }
  | ExprInfixLogic '||' Trivia ExprInfixCompare {
      infix!(tree,  $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::LogicalAnd)
    }
  | ExprInfixCompare { $1 }
  ;


ExprInfixCompare -> Result<AstRef>:
    ExprInfixCompare '<=' Trivia ExprInfixBitwise { infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::CmpLtEq)}
  | ExprInfixCompare '>=' Trivia ExprInfixBitwise { infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::CmpGtEq)}
  | ExprInfixCompare '==' Trivia ExprInfixBitwise { infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::CmpEq)}
  | ExprInfixCompare '!=' Trivia ExprInfixBitwise { infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::CmpNe)}
  | ExprInfixCompare '<' Trivia ExprInfixBitwise { infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::CmpLt)}
  | ExprInfixCompare '>' Trivia ExprInfixBitwise { infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::CmpGt)}
  | ExprInfixBitwise { $1 }
  ;

ExprInfixBitwise -> Result<AstRef>:
    Term '>>' Trivia Term {
      infix!(tree, $span, $1, trivia!(right trivia_tree, $5, trivia!(right trivia_tree, $5, $4)), InfixOp::BitRShift)
    }
  | Term '<<' Trivia Term {
      infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::BitLShift)
    }
  | Term '|' Trivia Term {
      infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::BitOr)
    }
  | Term '&' Trivia Term {
      infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::BitAnd)
    }
  | Term '^' Trivia Term {
      infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::BitXor)
    }
  | ExprInfixAdd { $1 }
  ;

ExprInfixAdd -> Result<AstRef>:
    ExprInfixAdd '-' Trivia ExprInfixMul {
      infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::Sub)
    }
  | ExprInfixAdd '+' Trivia ExprInfixMul {
      infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::Add)
    }
  | ExprInfixMul { $1 }
  ;

ExprInfixMul -> Result<AstRef>:
    ExprInfixMul '*' Trivia Term {
      infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::Mul)
    }
  | ExprInfixMul '/' Trivia Term {
      infix!(tree, $span, $1, trivia!(right trivia_tree, $5, $4), InfixOp::Div)
    }
  | ExprPrefix { $1 }
  ;

/// END: Infix Expressions


/// BEGIN: Prefix Expressions

ExprPrefixOnly -> Result<AstRef>:
  '-' Trivia Term { 
      trivia!(left trivia_tree, $2, node!(tree, $span, ExprPrefix { op: PrefixOp::Minus, rhs: $3? }))
    }
  | '+' Trivia Term { 
      trivia!(left trivia_tree, $2, node!(tree, $span, ExprPrefix { op: PrefixOp::Plus, rhs: $3? }))
    }
  | '~' Trivia Term { 
      trivia!(left trivia_tree, $2, node!(tree, $span, ExprPrefix { op: PrefixOp::BitNot, rhs: $3? }))
    }
  | '!' Trivia Term { 
      trivia!(left trivia_tree, $2, node!(tree, $span, ExprPrefix { op: PrefixOp::LogicalNot, rhs: $3? }))
    }
  | '*' Trivia Term {
      trivia!(left trivia_tree, $2, node!(tree, $span, ExprPrefix { op: PrefixOp::Deref, rhs: $3? }))
  }
  ;

ExprPrefix -> Result<AstRef>: ExprPrefixOnly { $1 } | Term { $1 };

/// END: Prefix Expressions

ExprFieldAccess -> Result<AstRef>:
    Term '.' IDENT Trivia {
      trivia!(right trivia_tree, $4,
        node!(tree, $span, FieldAccess {
          lhs: $1?,
          field: $lexer.span_str($3?.span()).into(),
        }))
    }
  ;

ExprArrayAccess -> Result<AstRef>:
    Term '[' Trivia Expr ']' Trivia {
      trivia!(right trivia_tree, $6,
        node!(tree, $span, ArrayAccess {
          lhs: $1?,
          index: trivia!(left trivia_tree, $3, $4?),
        }))
    }
  ;

Term -> Result<AstRef>:
    LiteralInt { $1 }
  | LiteralString { $1 }
  | LiteralChar { $1 }
  | LiteralStruct { $1 }
  | LiteralArray { $1 }
  | Ident { $1 }
  | ExprCall { $1 }
  | ExprArrayAccess { $1 }
  | ExprFieldAccess { $1 }
  | '(' Trivia ExprInfix ')' Trivia { 
    trivia!(
      left trivia_tree, $2, trivia!(right trivia_tree, $5, $3)
    )
  }
  ;

Ident -> Result<AstRef>:
    "IDENT" Trivia { trivia!(right trivia_tree, $2, node!(tree, $span, Ident { symbol: SmolStr::from($lexer.span_str($1?.span())) })) }
  ;

LiteralInt -> Result<AstRef>:
    _UInt Trivia { 
      trivia!(right trivia_tree, $4, node!(tree, $span, LiteralInteger {value: $1? }))
    }
  ;

LiteralChar -> Result<AstRef>:
    'CHAR' Trivia {
      // TODO: should have a separate production to handle any invalid char 'CHAR' only matches valid sequences
      trivia!(right trivia_tree, $2, node!(tree, $span, LiteralChar { value: unescape_char_literal($lexer.span_str($1?.span()))? }))
    }
  ;

LiteralString -> Result<AstRef>:
    'STRING' Trivia {
      // TODO: should have a separate production to handle any invalid char 'STRNG' only matches valid sequences
      trivia!(right trivia_tree, $2, node!(tree, $span, LiteralString { value: unescape_string_literal($lexer.span_str($1?.span()))? }))
    }
  ;

LiteralStruct -> Result<AstRef>:
    '#{' Trivia LiteralStructMemberListOpt '}' Trivia {
      trivia!(right trivia_tree, $10,
        node!(tree, $span,
          LiteralStruct { 
            members: $3?
          }))
    }
  ;

LiteralStructMemberListOpt -> Result<Vec<AstNode<LiteralStructMember>>>:
    LiteralStructMemberList ',' Trivia { 
      /* TODO: outer trivia */
      $1
    }
  | LiteralStructMemberList { $1 }
  | %empty { Ok(vec![]) }
  ;

LiteralStructMemberList -> Result<Vec<AstNode<LiteralStructMember>>>:
    LiteralStructMember { Ok(vec![$1?]) }
  | LiteralStructMemberList ',' Trivia LiteralStructMember {
      let mut arr = $1?;
      arr.push($4?);
      Ok(arr)
    }
  ;

LiteralStructMember -> Result<AstNode<LiteralStructMember>>:
    IDENT Trivia ':' Trivia Expr {
      Ok(AstNode {
        span: $span,
        data: LiteralStructMember {
          field: $lexer.span_str($1?.span()).into(),
          value: trivia!(left trivia_tree, $4, $5?)
        }
      })
    }
  ;


LiteralArray -> Result<AstRef>:
    '#[' Trivia LiteralArrayValueListOpt ']' Trivia {
      trivia!(right trivia_tree, $10,
        node!(tree, $span,
          LiteralArray { 
            values: $3?
          }))
    }
  ;

LiteralArrayValueListOpt -> Result<Vec<AstRef>>:
    LiteralArrayValueList ',' Trivia { 
      /* TODO: outer trivia */
      $1
    }
  | LiteralArrayValueList { $1 }
  | %empty { Ok(vec![]) }
  ;

LiteralArrayValueList -> Result<Vec<AstRef>>:
    Expr { Ok(vec![$1?]) }
  | LiteralArrayValueList ',' Trivia Expr {
      let mut arr = $1?;
      arr.push(trivia!(left trivia_tree, $3, $4?));
      Ok(arr)
    }
  ;


// Helper rule for numbers, simplifies LiteralInt grammar
_UInt -> Result<i128>:
    'UINT2'  { Ok(must_parse_int_radix::<2>($lexer.span_str($1?.span()))) }
  | 'UINT8'  { Ok(must_parse_int_radix::<8>($lexer.span_str($1?.span()))) }
  | 'UINT10' { Ok(must_parse_int_radix::<10>($lexer.span_str($1?.span()))) }
  | 'UINT16' { Ok(must_parse_int_radix::<16>($lexer.span_str($1?.span()))) };
 
/// BEGIN: Types
TyExpr -> Result<AstRef>:
    TyExprUnion { $1 }
  ;

TyExprUnion -> Result<AstRef>:
    TyExprUnion "|" Trivia TyTerm {
      node!(tree, $span, TyExprUnion { 
        lhs: $1?,
        rhs: trivia!(left trivia_tree, $3, $4?),
      })
    }
  | TyTerm { $1 }
  ;

TyStruct -> Result<AstRef>:
    '{' Trivia TyStructMemberList '}' Trivia {
        // TODO: inner trivia
        trivia!(right trivia_tree, $5,
          node!(tree, $span, TyStruct {
            members: $3?
          }))
    }
  | '{' Trivia '}' Trivia { 
    trivia!(right trivia_tree, $5,
      node!(tree, $span, TyStruct { members: Vec::new() })) }
  ;

TyStructMemberList -> Result<Vec<AstRef>>:
    TyStructMember { Ok(vec![$1?]) }
  | TyStructMemberList ',' Trivia TyStructMember {
      let mut arr = $1?;
      arr.push(trivia!(right trivia_tree, $3, $4?));
      Ok(arr)
    }
  ;

TyStructMember -> Result<AstRef>:
    'mut' IDENT Trivia ':' Trivia TyExpr {
      node!(tree, $span, TyStructMember {
        mutable: true,
        ty: trivia!(left trivia_tree, $5, $6?),
        name: $lexer.span_str($1?.span()).into()
      })
    }
  | IDENT Trivia ':' Trivia TyExpr {
      node!(tree, $span, TyStructMember {
        mutable: false,
        ty: trivia!(left trivia_tree, $4, $5?),
        name: $lexer.span_str($1?.span()).into()
      })
    }
  ;

TyIntegerRangeTerm -> Result<AstRef>:
    LiteralInt { $1 }
  ;

TyIntegerRange -> Result<AstRef>:
    TyIntegerRangeTerm { 
      let number = $1?;
      node!(tree, $span, TyNumberRange { lo: number.clone(), hi: number })
    }
  | TyIntegerRangeTerm '..' Trivia TyIntegerRangeTerm {
      node!(tree, $span, TyNumberRange { lo: $1?, hi: trivia!(left trivia_tree, $3, $4?) })
    }
  ;

TyArray -> Result<AstRef>: 
    '[' Trivia TyExpr ';' Trivia _UInt Trivia ']' Trivia {
      trivia!(right trivia_tree, $8,
        node!(tree, $span, TyArray { 
          element_ty: trivia!(left trivia_tree, $2?, $3?),
          length: trivia!(left trivia_tree, $5?, $6?)
        }))
    }
  ;

TySlice -> Result<AstRef>:
    '&[' Trivia TyExpr ';' Trivia TyExpr ']' Trivia {
      trivia!(right trivia_tree, $8,
        node!(tree, $span, TySlice { 
          element_ty: trivia!(left trivia_tree, $2, $3?),
          length_ty: trivia!(left trivia_tree, $5, $6?)
        }))
    }
  ;

TyRef -> Result<AstRef>:
    '&' Trivia TyTerm {
      node!(tree, $span, TyRef { 
        referenced_ty: trivia!(left trivia_tree, $2, $3?),
      })
    }
  ;

TyParamList -> Result<Vec<DefaultLinearTreeId>>:
    TyExpr { Ok(vec![$1?]) }
  | TyParamList ',' Trivia TyExpr {
      let mut arr = $1?;
      arr.push(trivia!(left trivia_tree, $3, $4?));
      Ok(arr)
    }
  ;

TyNamed -> Result<AstRef>:
    Ident {
      node!(tree, $span, TyNamed { 
        name: $1?,
        parameters: vec![],
      })
    }
  | Ident '[' Trivia TyParamList ']' Trivia {
      // TODO: inner trivia
      trivia!(right trivia_tree, $6,
        node!(tree, $span, TyNamed { 
          name: $1?,
          parameters: vec![],
        }))
    }
  ;

TyTerm -> Result<AstRef>:
    TyIntegerRange { $1 }
  | '(' Trivia TyExpr ')' Trivia { 
      // TODO: how to assoc final trivia
      trivia!(left trivia_tree, $2, $3)
    }
  | TyRef { $1 }
  | TySlice { $1 }
  | TyArray { $1 }
  | TyStruct { $1 }
  | TyNamed { $1 }
  | 'unit' Trivia { trivia!(right trivia_tree, $2, node!(tree, $span, TyUnit {})) }
  ;

/// END: Types

/// BEGIN: Trivia

Trivia -> Result<Option<Trivia>>:
    TriviaSeries { Ok(Some($1?)) }
  | %empty { Ok(None) }
  ;

TriviaRequired -> Result<Option<Trivia>>:
    TriviaSeries { Ok(Some($1?)) }
  ;

TriviaSeries -> Result<Trivia>:
    TriviaSeries TriviaPeice {
      let mut trivia = $1?;
      trivia.peices.push($2?);
      Ok(trivia)
  }
  | TriviaPeice { Ok(Trivia { peices: vec![$1?] }) }
  ;


TriviaPeice -> Result<TriviaPeice>:
  Newline { $1 }
  | Space { $1 }
  | Tab { $1 }
  | LineComment { $1 }
  | MultiLineComment { $1 }
  ;

Newline -> Result<TriviaPeice>:
   'LF'   { Ok(TriviaPeice::new($1?.span(), TriviaData::Newline(NewlineKind::Lf))) }
  |'CRLF' { Ok(TriviaPeice::new($1?.span(), TriviaData::Newline(NewlineKind::CrLf))) }
  ;

Space -> Result<TriviaPeice>:
  'SPACE' { Ok(TriviaPeice::new($1?.span(), TriviaData::Space)) };

Tab -> Result<TriviaPeice>:
  'TAB' { Ok(TriviaPeice::new($1?.span(), TriviaData::Tab)) };

LineComment -> Result<TriviaPeice>:
  'COMMENT-LINE' {
    Ok(TriviaPeice::new($span , TriviaData::Comment(CommentKind::Line)))
  };

MultiLineComment -> Result<TriviaPeice>:
  'COMMENT-MULTILINE' {
    Ok(TriviaPeice::new($span, TriviaData::Comment(CommentKind::MultiLine)))
  };

/// END: Trivia

%%

use crate::{Trivia, TriviaData, TriviaPeice, NewlineKind, tree::DefaultLinearTreeId, CommentKind, ast::*, string_codec::StringDecoder};
use allocator_api2::{vec::Vec, vec};
use smol_str::SmolStr;

pub type Result<T, E = Box<dyn std::error::Error>> = std::result::Result<T, E>;

#[inline(always)]
fn must_parse_int_radix<const RADIX: u32>(s: &str) -> i128 {
  let radix_prefix_size = if RADIX == 10 { 0 } else { 2 };
  let mut parsable = String::with_capacity(s.len());
  parsable.extend(s.chars().skip(radix_prefix_size).filter(|&c| c != '_'));

  i128::from_str_radix(&parsable, RADIX).unwrap()
}

#[inline(always)]
fn unescape_char_literal(token: &str) -> Result<char> {
  // remove quotes from token
  let dec = StringDecoder::new_for_char(&token[1..token.len() - 1]);
  Ok(dec.compile().map_err(|e| Box::new(e))?.chars().nth(0).unwrap())
}

#[inline(always)]
fn unescape_string_literal(token: &str) -> Result<SmolStr> {
  // remove quotes from token
  let dec = StringDecoder::new_for_string(&token[1..token.len() - 1]);
  Ok(dec.compile().map_err(|e| Box::new(e))?)
}

pub type AstRef = DefaultLinearTreeId;

macro_rules! node {
  ($t:ident, $span:expr, $node:expr) => {
    Ok($t.push(AstNode::new($span, $node)))
  }
}
macro_rules! infix {
  ($tree:ident, $span:ident, $lhs:expr, $rhs:expr, $op:expr) => {
    node!($tree, $span, ExprInfix { lhs: $lhs?, rhs: $rhs?, op: $op })
  }
}

macro_rules! trivia {
  (left $trivia_map:ident, $trivia:expr, $node:expr) => {
    $node
  };

  (right $trivia_map:ident, $trivia:expr, $node:expr) => {
    $node
  }
}