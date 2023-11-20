# Changes in Input Text Parsing

### Main Execution Loop:
Modified the main function to execute a loop over 'e'.

### Read Input Function:
Updated the 'read_input' function to read the input until the string ";;" is encountered.
- In cases where ";;" is absent, the strings are continually added to the list.

### Tokenization and Loop Execution:
1. Tokenized the obtained list.
2. Initiated a loop execution using the obtained context.

# Integrating Recursion

1. **New Term Addition:**
   - Introduced `TmFix of term` for handling recursion within function definitions.
   - Updated module interface and type declarations within the module.

2. **Pattern Matching Corrections:**
   - Revised pattern matching across existing functions for consistency.

3. **Recursion Syntax Inclusion:**
   - Integrated the syntax for recursion (`letrec f : ... = ... in f ...`).
   - Implemented the necessary terms to declare recursion, excluding the `letrec` itself.

4. **Lexer and Type Addition:**
   - Expanded lexer to incorporate the `letrec` term and its structured form.
   - Implemented typing (`$4`), ensuring typed variables.

5. **Specific Changes in Functions:**
- Modified functions selectively:
  1. Altered `typeof` for checking if `t1` is a function with identical domain and codomain.
  2. Minimal alterations in the term manager, free variables list, and substitution functions.
  3. Revised `eval` by adding evaluation rules as outlined in the provided document.

6. **Changes in Evaluating Functions:**
- Introduced `E-Fix` conditionally, considering when `t1` is incompletely evaluated.
- Implemented `E-FixBeta` to replace 'x' with the entire expression in `t2`.

# Addition of String Variables

1. **Term Expansion:**
   - Added `TmString of string` and `TmConcat of term * term` to the existing terms.

2. **Type Addition:**
   - Included the type `TmString`.

3. **Lexer and Parser Adjustments:**
   - Updated lexer and parser to accept specific user inputs (`concat` and multiline strings with `\n`).
   - Revised parser for `STRING`, `CONCAT`, and `STRINGV`.

4. **Syntax Rule and Term Modification:**
   - Introduced a new syntactic rule: `CONCAT`.
   - Added the atomic term `STRINGV`.

5. **Type Management Adaptation:**
   - Introduced `TyString` within the .ml file.
   - Adjusted type manager to accommodate the new changes.

6. **Handling New Term Construction:**
   - Incorporated construction cases for the new types (`TmString` and `TmConcat`), verifying both as `TmString` type.

7. **String Representation Modification:**
   - Updated `string_of_term` to reflect the changes made.

8. **Evaluation Rules for TmConcat:**
   - Included evaluation rules for `TmConcat`, resulting in `TmString(s1 ^ s2)`.