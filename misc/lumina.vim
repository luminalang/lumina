" Vim syntax file
" Language: 		Lumina
" Maintainer:		simvux
" Last Change:		2023 Jan 31 by Simon Larsson <simon.larsson-general@protonmail.com>
" Original Author:	Simon Larsson <simon.larsson-general@protonmail.com>
" Mostly taken from the fantastic Rust syntax file, thank you for your original work

if exists("b:current_syntax")
  finish
endif

" Syntax definitions {{{1
" Basic keywords {{{2
syn keyword   luminaConditional match if else
syn keyword   luminaTypedef type nextgroup=luminaIdentifier skipwhite skipempty

syn keyword   luminaKeyword     fn nextgroup=luminaFuncName skipwhite skipempty
syn keyword   luminaKeyword     in impl let where when do then as can val
syn keyword   luminaKeyword     pub hid nextgroup=luminaPubScope skipwhite skipempty
syn keyword   luminaKeyword     use skipwhite skipempty
syn keyword   luminaKeyword     mod trait nextgroup=luminaIdentifier skipwhite skipempty
syn match luminaDefault /\<default\ze\_s\+\(impl\|fn\|type\)\>/

syn keyword   luminaInvalidBareKeyword crate

syn match luminaPubScopeDelim /[()]/ contained
syn match luminaPubScope /([^()]*)/ contained contains=luminaPubScopeDelim,luminaSuper,luminaModPath,luminaModPathSep,transparent

syn match     luminaIdentifier  contains=luminaIdentifierPrime "\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*" display contained
syn match     luminaFuncName    "\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\)\%([^[:cntrl:][:punct:][:space:]]\|_\)*" display contained

syn region    luminaBoxPlacement matchgroup=luminaBoxPlacementParens start="(" end=")" contains=TOP contained
syn region    luminaBoxPlacementBalance start="(" end=")" containedin=luminaBoxPlacement transparent
syn region    luminaBoxPlacementBalance start="\[" end="\]" containedin=luminaBoxPlacement transparent

syn keyword   luminaType        int uint char bool u8 u16 u32 u64 u128 f32 string option result
syn keyword   luminaType        f64 i8 i16 i32 i64 i128 self

syn keyword   luminaBoolean     true false

" If foo::bar changes to foo.bar, change this ("::" to "\.").
" If foo::bar changes to Foo::bar, change this (first "\w" to "\u").
syn match     luminaModPath     "\w\(\w\)*:[^<]"he=e-2,me=e-2
syn match     luminaModPathSep  ":"

syn match     luminaFuncCall    "\w\(\w\)*("he=e-1,me=e-1

"syn match     luminaCapsIdent    display "[A-Z]\w\(\w\)*"

syn match     luminaOperator     display "\%(\\\|!\|+\|/\|*\|&\|%\|@\|$\|?\|^\|<\|>\|=\||\)=\?"
syn match     luminaArrowCharacter display "->"

syn match     luminaEscapeError   display contained /\\./
syn match     luminaEscape        display contained /\\\([nrt0\\'"]\|x\x\{2}\)/
syn match     luminaEscapeUnicode display contained /\\u{\x\{1,6}}/
syn match     luminaStringContinuation display contained /\\\n\s*/
syn region    luminaString      start=+b"+ skip=+\\\\\|\\"+ end=+"+ contains=luminaEscape,luminaEscapeError,luminaStringContinuation
syn region    luminaString      start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=luminaEscape,luminaEscapeUnicode,luminaEscapeError,luminaStringContinuation,@Spell
syn region    luminaString      start='b\?r\z(#*\)"' end='"\z1' contains=@Spell

" Number literals
syn match     luminaDecNumber   display "\<[0-9][0-9_]*\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
syn match     luminaHexNumber   display "\<0x[a-fA-F0-9_]\+\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
syn match     luminaOctNumber   display "\<0o[0-7_]\+\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="
syn match     luminaBinNumber   display "\<0b[01_]\+\%([iu]\%(size\|8\|16\|32\|64\|128\)\)\="

" Special case for numbers of the form "1." which are float literals, unless followed by
" an identifier, which makes them integer literals with a method call or field access,
" or by another ".", which makes them integer literals followed by the ".." token.
" (This must go first so the others take precedence.)
syn match     luminaFloat       display "\<[0-9][0-9_]*\.\%([^[:cntrl:][:space:][:punct:][:digit:]]\|_\|\.\)\@!"
" To mark a number as a normal float, it must have at least one of the three things integral values don't have:
" a decimal point and more numbers; an exponent; and a type suffix.
syn match     luminaFloat       display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\%([eE][+-]\=[0-9_]\+\)\=\(f32\|f64\)\="
syn match     luminaFloat       display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\=\%([eE][+-]\=[0-9_]\+\)\(f32\|f64\)\="
syn match     luminaFloat       display "\<[0-9][0-9_]*\%(\.[0-9][0-9_]*\)\=\%([eE][+-]\=[0-9_]\+\)\=\(f32\|f64\)"

syn region luminaCommentLine                                                  start="//"                      end="$"   contains=luminaTodo,@Spell
syn region luminaCommentLineDoc                                               start="//\%(//\@!\|!\)"         end="$"   contains=luminaTodo,@Spell
syn region luminaCommentLineDocError                                          start="//\%(//\@!\|!\)"         end="$"   contains=luminaTodo,@Spell contained
syn region luminaCommentBlock             matchgroup=luminaCommentBlock         start="/\*\%(!\|\*[*/]\@!\)\@!" end="\*/" contains=luminaTodo,luminaCommentBlockNest,@Spell
syn region luminaCommentBlockDoc          matchgroup=luminaCommentBlockDoc      start="/\*\%(!\|\*[*/]\@!\)"    end="\*/" contains=luminaTodo,luminaCommentBlockDocNest,@Spell
syn region luminaCommentBlockDocError     matchgroup=luminaCommentBlockDocError start="/\*\%(!\|\*[*/]\@!\)"    end="\*/" contains=luminaTodo,luminaCommentBlockDocNestError,@Spell contained
syn region luminaCommentBlockNest         matchgroup=luminaCommentBlock         start="/\*"                     end="\*/" contains=luminaTodo,luminaCommentBlockNest,@Spell contained transparent
syn region luminaCommentBlockDocNest      matchgroup=luminaCommentBlockDoc      start="/\*"                     end="\*/" contains=luminaTodo,luminaCommentBlockDocNest,@Spell contained transparent
syn region luminaCommentBlockDocNestError matchgroup=luminaCommentBlockDocError start="/\*"                     end="\*/" contains=luminaTodo,luminaCommentBlockDocNestError,@Spell contained transparent
" FIXME: this is a really ugly and not fully correct implementation. Most
" importantly, a case like ``/* */*`` should have the final ``*`` not being in
" a comment, but in practice at present it leaves comments open two levels
" deep. But as long as you stay away from that particular case, I *believe*
" the highlighting is correct. Due to the way Vim's syntax engine works
" (greedy for start matches, unlike Rust's tokeniser which is searching for
" the earliest-starting match, start or end), I believe this cannot be solved.
" Oh you who would fix it, don't bother with things like duplicating the Block
" rules and putting ``\*\@<!`` at the start of them; it makes it worse, as
" then you must deal with cases like ``/*/**/*/``. And don't try making it
" worse with ``\%(/\@<!\*\)\@<!``, either...

syn keyword luminaTodo contained TODO FIXME XXX NB NOTE

" Folding rules {{{2
" Trivial folding rules to begin with.
" FIXME: use the AST to make really good folding
syn region luminaFoldBraces start="{" end="}" transparent fold

" Default highlighting {{{1
hi def link luminaDecNumber       luminaNumber
hi def link luminaHexNumber       luminaNumber
hi def link luminaOctNumber       luminaNumber
hi def link luminaBinNumber       luminaNumber
hi def link luminaIdentifierPrime luminaIdentifier
hi def link luminaTrait           luminaType
hi def link luminaDeriveTrait     luminaTrait

hi def link luminaEscape        Special
hi def link luminaEscapeUnicode luminaEscape
hi def link luminaEscapeError   Error
hi def link luminaStringContinuation Special
hi def link luminaString        String
hi def link luminaNumber        Number
hi def link luminaBoolean       Boolean
hi def link luminaEnum          luminaType
hi def link luminaEnumVariant   luminaConstant
hi def link luminaConstant      Constant
hi def link luminaFloat         Float
hi def link luminaArrowCharacter luminaOperator
hi def link luminaOperator      Operator
hi def link luminaKeyword       Keyword
hi def link luminaTypedef       Keyword " More precise is Typedef, but it doesn't feel right for Rust
hi def link luminaStructure     Keyword " More precise is Structure
hi def link luminaPubScopeDelim Delimiter
hi def link luminaConditional   Conditional
hi def link luminaIdentifier    Identifier
hi def link luminaCapsIdent     luminaIdentifier
hi def link luminaModPath       Include
hi def link luminaModPathSep    Delimiter
hi def link luminaFunction      Function
hi def link luminaFuncName      Function
hi def link luminaFuncCall      Function
hi def link luminaCommentLine   Comment
hi def link luminaCommentLineDoc SpecialComment
hi def link luminaCommentLineDocError Error
hi def link luminaCommentBlock  luminaCommentLine
hi def link luminaCommentBlockDoc luminaCommentLineDoc
hi def link luminaCommentBlockDocError Error
hi def link luminaType          Type
hi def link luminaTodo          Todo
hi def link luminaDefault       StorageClass
hi def link luminaStorage       StorageClass
hi def link luminaObsoleteStorage Error
hi def link luminaLabel         Label
hi def link luminaInvalidBareKeyword Error

syn sync minlines=200
syn sync maxlines=500

let b:current_syntax = "lumina"
