#!/usr/bin/env python3
"""Fill in TODO FILL IN placeholders in Quotes.scala with contextual documentation."""

import re


def get_param_description(name, context, lineno):
    """Generate a description for a @param based on its name and context."""

    # Common pattern: unapply methods - `x` is the tree/value to match
    if name == 'x':
        if 'unapply' in context:
            unapply_match = re.search(r'def unapply\(x:\s*(\w+)\)', context)
            if unapply_match:
                type_name = unapply_match.group(1)
                return f'the `{type_name}` to match against'
        if 'apply' in context:
            apply_match = re.search(r'def apply\(x:\s*(\w+)\)', context)
            if apply_match:
                type_name = apply_match.group(1)
                return f'the `{type_name}` value'
        return 'the value to match against'

    if name == 'constant':
        if 'unapply' in context:
            for const_type in ['BooleanConstant', 'ByteConstant', 'ShortConstant', 'IntConstant',
                              'LongConstant', 'FloatConstant', 'DoubleConstant', 'CharConstant',
                              'StringConstant', 'UnitConstant', 'NullConstant', 'ClassOfConstant']:
                if const_type in context:
                    return f'the `{const_type}` to match against'
            return 'the constant to match against'
        if 'apply' in context:
            if 'Literal' in context:
                return 'the constant value for the literal'
            return 'the constant value'

    if name == 'Printer':
        return 'the printer used to render the value as a `String`'

    if name == 'Type':
        return 'the evidence that the type has a valid representation'

    if name == 'FromExpr':
        return 'the typeclass instance used to extract the value'

    if name == 'tree':
        return 'the tree to match against'

    if name == 'original':
        return 'the original tree being copied'

    if name == 'pid':
        return 'the package name reference'

    if name == 'stats':
        return 'the list of statements'

    if name == 'expr':
        if 'report' in context or ('error(' in context and 'msg' in context) or ('warning(' in context and 'msg' in context) or ('info(' in context and 'msg' in context):
            return 'the expression whose position is used for reporting'
        if 'Import' in context or 'Export' in context:
            return 'the qualifier expression'
        if 'Block' in context:
            return 'the result expression of the block'
        if 'Typed' in context or 'TypedOrTest' in context:
            return 'the expression being ascribed'
        if 'Try' in context:
            return 'the body expression of the try block'
        if 'Return' in context:
            return 'the expression being returned'
        return 'the expression term'

    if name == 'selectors':
        return 'the list of import/export selectors'

    if name == 'symbol':
        return 'the symbol of the member to select'

    if name == 'qualifier':
        return 'the qualifier term'

    if name == 'name':
        if 'NamedArg' in context:
            return 'the argument name'
        if 'Select' in context:
            return 'the name of the member to select'
        if 'declaredField' in context or 'fieldMember' in context:
            return 'the name of the field'
        if 'declaredMethod' in context or 'methodMember' in context:
            return 'the name of the method'
        if 'declaredType' in context or 'typeMember' in context:
            return 'the name of the type member'
        if 'ValDef' in context and 'let' in context:
            return 'the name of the `val` binding'
        return 'the name to look up'

    if name == 'fun':
        if 'Unapply' in context:
            return 'the extractor function'
        if 'Apply' in context:
            return 'the function being applied'
        if 'TypeApply' in context:
            return 'the function being type-applied'
        return 'the function term'

    if name == 'args':
        if 'TypeApply' in context:
            return 'the type argument trees'
        if 'AppliedType' in context:
            return 'the type arguments'
        return 'the term arguments'

    if name == 'arg':
        if 'NamedArg' in context:
            return 'the argument value'
        return 'the argument term'

    if name == 'tpt':
        if 'New' in context:
            return 'the type tree of the class to instantiate'
        if 'Typed' in context or 'TypedOrTest' in context:
            return 'the type tree for the ascription'
        if 'Repeated' in context:
            return 'the element type tree'
        return 'the type tree'

    if name == 'cls':
        if 'This' in context:
            return 'the symbol of the enclosing class'
        if 'baseType' in context:
            return 'the class symbol to compute the base type for'
        if 'derivesFrom' in context:
            return 'the class symbol to check against'
        return 'the class symbol'

    if name == 'targs':
        return 'the type arguments'

    if name == 'returnType':
        return 'the expected return type for overload resolution'

    if name == 'lhs':
        return 'the left-hand side of the assignment'

    if name == 'rhs':
        if 'Assign' in context:
            return 'the right-hand side of the assignment'
        if 'ValDef' in context:
            return 'the right-hand side term'
        return 'the right-hand side expression'

    if name == 'cond':
        return 'the condition expression'

    if name == 'thenp':
        return 'the then branch expression'

    if name == 'elsep':
        return 'the else branch expression'

    if name == 'body':
        if 'While' in context:
            return 'the loop body expression'
        if 'let' in context:
            return 'a function that takes a reference to the bound value and returns the body term'
        return 'the body expression'

    if name == 'selector':
        if 'Match' in context:
            return 'the scrutinee expression being matched'
        return 'the selector expression'

    if name == 'cases':
        if 'SummonFrom' in context:
            return 'the list of match cases for the given match'
        if 'Try' in context:
            return 'the list of catch case definitions'
        return 'the list of case definitions'

    if name == 'finalizer':
        return 'the optional finally block expression'

    if name == 'from':
        if 'Return' in context:
            return 'the symbol of the enclosing method'
        if 'substituteTypes' in context:
            return 'the list of symbols to substitute'
        return 'the source'

    if name == 'to':
        if 'substituteTypes' in context:
            return 'the list of replacement types corresponding to `from`'
        return 'the target'

    if name == 'elems':
        return 'the list of element terms in the sequence'

    if name == 'qual':
        return 'the qualifier term'

    if name == 'mix':
        return 'the optional mixin class name for `super[mix]`'

    if name == 'wildcard':
        return 'the wildcard tree to match against'

    if name == 'implicits':
        return 'the implicit arguments passed to the extractor'

    if name == 'patterns':
        return 'the list of nested patterns'

    if name == 'owner':
        return 'the owner symbol'

    if name == 'terms':
        return 'the list of right-hand side terms to bind'

    if name == 'tp':
        return 'the type to use'

    if name == 'sym':
        if 'isTupleClass' in context:
            return 'the symbol to check'
        if 'select' in context:
            return 'the member symbol to select'
        return 'the symbol to use'

    if name == 'tycon':
        return 'the type constructor'

    if name == 'parentExp':
        return 'a function that takes the recursive type itself and returns its underlying type'

    if name == 'member':
        return 'the member symbol whose type is to be computed'

    if name == 'idx':
        return 'the zero-based index of the parameter'

    if name == 'targ':
        return 'the type argument to apply'

    if name == 'clazz':
        return 'the runtime class'

    if name == 'path':
        return 'the fully qualified name (e.g., `scala.collection.immutable.List`)'

    if name == 'fullName':
        return 'the fully qualified name of the class'

    if name == 'arity':
        return 'the arity of the tuple'

    if name == 'sig':
        return 'the signature to match against'

    if name == 'sourceFile':
        return 'the source file for the position'

    if name == 'start':
        return 'the start offset in the source file'

    if name == 'end':
        return 'the end offset in the source file'

    if name == 'annotSym':
        if 'hasAnnotation' in context:
            return 'the symbol of the annotation to check for'
        if 'getAnnotation' in context:
            return 'the symbol of the annotation to retrieve'
        return 'the annotation symbol'

    if name == 'tpe':
        if 'ClassOfConstant' in context:
            return 'the type represented by the `classOf` constant'
        return 'the type'

    if name == 'msg':
        return 'the message text to report'

    if name == 'pos':
        return 'the position at which to report the message'

    if name == 'that':
        if 'Flags' in context:
            return 'the flag set to combine with'
        return 'the other value'

    if name == 'rhsFn':
        return 'a function that takes references to the parameters and returns the method body'

    if name == 'Quotes':
        return 'the quotation context provided by the enclosing splice'

    if name == 'q':
        return 'the implicit `Quotes` instance in scope'

    # Fallback
    return f'the `{name}` parameter'


def get_tparam_description(name, context, lineno):
    """Generate a description for a @tparam based on its name and context."""

    if name == 'T':
        if 'TypeTree' in context and 'of[T' in context:
            return 'the type or kind to create a `TypeTree` for'
        if 'TypeRepr' in context and 'of[T' in context:
            return 'the type or kind to get the `TypeRepr` for'
        if 'Printer' in context:
            return 'the type of value that this printer can render'
        return 'the type parameter'

    if name == 'X':
        if 'TreeAccumulator' in context:
            return 'the type of the accumulated value'
        return 'the type parameter'

    return f'the type parameter `{name}`'


def get_return_description(context, lineno):
    """Generate a description for a @return based on context."""

    if 'searchIgnoring' in context:
        return 'the result of the implicit search'

    if 'selectors' in context:
        if 'Import' in context:
            return 'the list of import selectors'
        return 'the list of export selectors'

    if 'self: Option[ValDef]' in context:
        return 'the self-type `ValDef` if one is declared, or `None`'

    if 'body: List[Statement]' in context:
        return 'the list of statements in the class body'

    if 'leadingTypeParams' in context:
        return 'the leading type parameter definitions'

    if 'trailingParamss' in context:
        return 'the parameter clauses following leading type parameters'

    if 'underlyingArgument' in context:
        return 'the underlying term'

    if 'underlying: Term' in context:
        return 'the underlying term'

    if 'def fun: Term' in context:
        if 'TypeApply' in context:
            return 'the function part of the type application'
        return 'the function part of the application'

    if 'def args: List[Term]' in context:
        return 'the list of term arguments'

    if 'def args: List[TypeTree]' in context:
        return 'the list of type argument trees'

    if 'def id: Option[String]' in context:
        return 'the optional qualifying class name'

    if 'Unapply' in context and 'def fun' in context:
        return 'the extractor function term'

    if 'asType' in context:
        return 'a `Type[?]` representation of this type'

    if 'widen' in context:
        return 'the widened type'

    if 'memberType' in context:
        return 'the type of the member as seen from this prefix type'

    if 'newMethod' in context:
        return 'the newly created method symbol'

    if 'newVal' in context:
        return 'the newly created val symbol'

    if 'newBind' in context:
        return 'the newly created bind symbol'

    if 'newClass' in context:
        return 'the newly created class symbol'

    if 'newModule' in context:
        return 'the newly created module symbol'

    if 'newTypeAlias' in context:
        return 'the newly created type alias symbol'

    if 'newBoundedType' in context:
        return 'the newly created bounded type symbol'

    if 'freshName' in context:
        return 'a fresh name with the given prefix'

    if 'spliceOwner' in context:
        return 'the symbol of the definition enclosing the current macro expansion splice'

    if 'def tree: Tree' in context:
        return 'the definition tree of this symbol'

    if 'asQuotes' in context:
        return 'a nested `Quotes` instance with this symbol as the splice owner'

    if 'typeRef' in context:
        return 'a `TypeRef` to this type symbol'

    if 'paramSigs' in context:
        return 'the list of parameter signatures'

    if 'paramVariance' in context and 'paramVariances' not in context:
        return 'the variance flags for this type parameter'

    if 'paramVariances' in context:
        return 'the list of variance flags, one per type parameter'

    if 'RecursiveType' in context and 'apply' in context:
        return 'a new `RecursiveType`'

    if 'AbsOverride' in context:
        return 'the `AbsOverride` flag'

    if 'path: String' in context and 'SourceFile' in context:
        return 'the path string of this source file'

    if 'ClassDef' in context and 'apply' in context:
        return 'a new `ClassDef` tree'

    if 'ValDef' in context and 'let' in context:
        return 'a block containing the val definition(s) and the body'

    if 'betaReduce' in context:
        return '`Some` with the beta-reduced term, or `None` if not reducible'

    # Fallback
    return 'the result'


def main():
    with open('/workspace/scala3/library/src/scala/quoted/Quotes.scala', 'r') as f:
        lines = f.readlines()

    replacements = {}

    for i, line in enumerate(lines):
        if 'TODO FILL IN' not in line:
            continue

        # Skip non-doc TODO comments (like "// TODO update when..." or "// TODO add selfOpt")
        stripped = line.strip()
        if stripped.startswith('//'):
            continue

        lineno = i + 1
        param_match = re.search(r'@param\s+(\w+)\s+TODO FILL IN', stripped)
        tparam_match = re.search(r'@tparam\s+(\w+)\s+TODO FILL IN', stripped)
        return_match = re.search(r'@return\s+TODO FILL IN', stripped)

        if param_match:
            param_name = param_match.group(1)
            context_lines = []
            for j in range(max(0, i-15), min(len(lines), i+20)):
                context_lines.append(lines[j].rstrip())
            context = '\n'.join(context_lines)

            desc = get_param_description(param_name, context, lineno)
            new_line = line.replace(f'@param {param_name} TODO FILL IN', f'@param {param_name} {desc}')
            replacements[i] = new_line

        elif tparam_match:
            tparam_name = tparam_match.group(1)
            context_lines = []
            for j in range(max(0, i-15), min(len(lines), i+20)):
                context_lines.append(lines[j].rstrip())
            context = '\n'.join(context_lines)

            desc = get_tparam_description(tparam_name, context, lineno)
            new_line = line.replace(f'@tparam {tparam_name} TODO FILL IN', f'@tparam {tparam_name} {desc}')
            replacements[i] = new_line

        elif return_match:
            context_lines = []
            for j in range(max(0, i-20), min(len(lines), i+20)):
                context_lines.append(lines[j].rstrip())
            context = '\n'.join(context_lines)

            desc = get_return_description(context, lineno)
            new_line = line.replace('@return TODO FILL IN', f'@return {desc}')
            replacements[i] = new_line

    # Apply replacements
    for i in sorted(replacements.keys()):
        lines[i] = replacements[i]

    with open('/workspace/scala3/library/src/scala/quoted/Quotes.scala', 'w') as f:
        f.writelines(lines)

    print(f"Applied {len(replacements)} replacements")


if __name__ == '__main__':
    main()
