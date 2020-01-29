#!/usr/bin/env node
import * as ts from 'typescript';
import * as yargs from 'yargs';

function createSimpleParameter(name: string | ts.Identifier): ts.ParameterDeclaration {
  return ts.createParameter(/*decorators*/ undefined, /*modifiers*/ undefined, /*dotDotDotToken*/ undefined, name);
}

interface Arguments extends yargs.Arguments {
  tsconfig: string;
  debug: boolean;
  _: string[];
}

const argv = yargs
  .option('tsconfig', {
    describe: 'path to tsconfig file',
    default: 'tsconfig.json',
    type: 'string',
  })
  .option('debug', {
    describe: 'debug mode',
    default: false,
    type: 'boolean',
  })
  .demandCommand(1).argv;

class Generator {
  private fileNames: string[];
  private program: ts.Program;
  private typeChecker: ts.TypeChecker;
  private compilerHost: ts.CompilerHost;
  private printer: ts.Printer;
  private outFile: ts.SourceFile;

  private rootSourceFiles: ts.SourceFile[];
  private functions: ts.FunctionDeclaration[];
  private interfaces: { [key: string]: ts.InterfaceDeclaration };
  private debug = false;

  constructor(argv: Arguments) {
    const fileNames = argv._;
    this.fileNames = Array.isArray(fileNames) ? fileNames : [fileNames];

    const options = this.getCompilerOptions(argv.tsconfig);
    this.debug = argv.debug;
    this.compilerHost = ts.createCompilerHost(options);
    this.program = ts.createProgram(this.fileNames, options, this.compilerHost);
    this.typeChecker = this.program.getTypeChecker();
    this.functions = [];
    this.interfaces = {};
    this.printer = ts.createPrinter({ newLine: ts.NewLineKind.LineFeed });
    this.outFile = ts.createSourceFile('someFileName.ts', '', ts.ScriptTarget.Latest, /*setParentNodes*/ false, ts.ScriptKind.TS);
  }

  private getCompilerOptions(tsConfigFile: string): ts.CompilerOptions {
    const parseConfigHost: ts.ParseConfigHost = {
      fileExists: ts.sys.fileExists,
      readFile: ts.sys.readFile,
      readDirectory: ts.sys.readDirectory,
      useCaseSensitiveFileNames: true,
    };

    const configFileName = ts.findConfigFile('./', ts.sys.fileExists, tsConfigFile);
    if (!configFileName) {
      return {
        ...ts.getDefaultCompilerOptions(),
        typeRoots: [] /* FIXME */,
      };
    }
    const configFile = ts.readConfigFile(configFileName, ts.sys.readFile);
    return ts.parseJsonConfigFileContent(configFile.config, parseConfigHost, './').options;
  }

  createInterface(node: ts.TypeReferenceNode): ts.TypeReferenceNode {
    const theType = this.typeChecker.getTypeAtLocation(node);
    const decl = theType.symbol.declarations[0];
    if (ts.isInterfaceDeclaration(decl)) {
      const ifaceName = decl.name.escapedText as string;

      // TODO: Only support interface defined in the same file for now.
      if (!this.rootSourceFiles.includes(decl.getSourceFile())) {
        console.error(`Warning: Interface '${ifaceName}' is not declared in the specified source files.`);
        return node;
      }

      if (!this.interfaces[ifaceName]) {
        const newModifiers = decl.modifiers?.slice() ?? [];
        newModifiers?.push(ts.createToken(ts.SyntaxKind.ExportKeyword));
        this.interfaces[ifaceName] = ts.createInterfaceDeclaration(decl.decorators, newModifiers, decl.name, decl.typeParameters, decl.heritageClauses, decl.members);
      }

      return ts.createTypeReferenceNode(decl.name, []);
    } else {
      console.error(`Warning: Unexpected node kind ${ts.SyntaxKind[decl.kind]}`);
      return node;
    }
  }

  resolveType(t: ts.TypeNode | undefined): ts.TypeNode {
    if (!t) {
      return ts.createKeywordTypeNode(ts.SyntaxKind.VoidKeyword);
    }

    if (ts.isTypeReferenceNode(t)) {
      return this.createInterface(t);
    }

    if (ts.isUnionTypeNode(t) || ts.isIntersectionTypeNode(t)) {
      return ts.createUnionOrIntersectionTypeNode(
        t.kind,
        t.types.map(elementType => this.resolveType(elementType)),
      );
    }

    if (ts.isArrayTypeNode(t)) {
      return ts.createArrayTypeNode(this.resolveType(t.elementType));
    }

    switch (t.kind) {
      case ts.SyntaxKind.AnyKeyword:
      case ts.SyntaxKind.UnknownKeyword:
      case ts.SyntaxKind.NumberKeyword:
      case ts.SyntaxKind.BigIntKeyword:
      case ts.SyntaxKind.ObjectKeyword:
      case ts.SyntaxKind.BooleanKeyword:
      case ts.SyntaxKind.StringKeyword:
      case ts.SyntaxKind.SymbolKeyword:
      case ts.SyntaxKind.ThisKeyword:
      case ts.SyntaxKind.VoidKeyword:
      case ts.SyntaxKind.UndefinedKeyword:
      case ts.SyntaxKind.NullKeyword:
      case ts.SyntaxKind.NeverKeyword:
        return t;
    }

    console.error(`Warning: Unexpected return type '${ts.SyntaxKind[t.kind]}'.`);
    if (this.debug) {
      console.error(t);
    }
    return t;
  }

  createAsyncFunction(functionName: ts.Identifier, parameters: ts.NodeArray<ts.ParameterDeclaration> | undefined, returnType: ts.TypeNode | undefined): ts.FunctionDeclaration {
    const promise = ts.createIdentifier('Promise');
    const resolve = ts.createIdentifier('resolve');
    const reject = ts.createIdentifier('reject');

    const newParameters = (parameters ?? ts.createNodeArray()).map(p => {
      const newTypeNode = this.resolveType(p.type);
      return ts.createParameter(p.decorators, p.modifiers, p.dotDotDotToken, p.name, p.questionToken, newTypeNode, p.initializer);
    });

    let expression: ts.Expression = ts.createIdentifier('google');
    expression = ts.createPropertyAccess(expression, 'script');
    expression = ts.createPropertyAccess(expression, 'run');
    expression = ts.createCall(ts.createPropertyAccess(expression, 'withSuccessHandler'), undefined, [resolve]);
    expression = ts.createCall(ts.createPropertyAccess(expression, 'withFailureHandler'), undefined, [reject]);
    expression = ts.createCall(
      ts.createPropertyAccess(expression, functionName),
      undefined,
      newParameters.map(p => p.name as ts.Identifier),
    );
    const executor = ts.createArrowFunction(
      /*modifiers*/ undefined,
      /*typeParameters*/ undefined,
      [createSimpleParameter('resolve'), createSimpleParameter('reject')],
      /*type*/ undefined,
      /*equalsGreaterThanToken*/ undefined,
      /*body*/ ts.createBlock([ts.createExpressionStatement(expression)], /*multiline*/ true),
    );

    const statements = [ts.createReturn(ts.createNew(promise, undefined, [executor]))];

    return ts.createFunctionDeclaration(
      /*decorators*/ undefined,
      /*modifiers*/ ts.createNodeArray([ts.createToken(ts.SyntaxKind.ExportKeyword), ts.createToken(ts.SyntaxKind.AsyncKeyword)]),
      /*asteriskToken*/ undefined,
      functionName,
      /*typeParameters*/ undefined,
      newParameters,
      /*returnType*/ ts.createTypeReferenceNode(promise, [this.resolveType(returnType)]),
      ts.createBlock(statements, /*multiline*/ true),
    );
  }

  print(node: ts.Node): void {
    const result = this.printer.printNode(ts.EmitHint.Unspecified, node, this.outFile);
    console.log(result);
  }

  addExportFunction(functionName: ts.Identifier, functionNode: ts.FunctionLikeDeclaration): void {
    const fn = this.createAsyncFunction(functionName, functionNode.parameters, functionNode.type);
    this.functions.push(fn);
  }

  do(): void {
    const sourceFiles = this.program.getSourceFiles();

    const diagnostics = ts.getPreEmitDiagnostics(this.program);
    if (diagnostics.length > 0) {
      console.error(ts.formatDiagnosticsWithColorAndContext(diagnostics, this.compilerHost));
      process.exit(1);
    }

    this.rootSourceFiles = sourceFiles.filter(sf => this.fileNames.includes(sf.fileName));

    this.rootSourceFiles
      .map(sf => this.typeChecker.getSymbolAtLocation(sf))
      .filter((s): s is ts.Symbol => !!s)
      .map(s => this.typeChecker.getExportsOfModule(s))
      .reduce((acc, s) => acc.concat(s), [])
      .forEach(symbol => {
        const node = symbol.declarations[0];
        if (node.kind === ts.SyntaxKind.VariableDeclaration) {
          const varNode = node as ts.VariableDeclaration;
          if (varNode.initializer && varNode.initializer.kind === ts.SyntaxKind.ArrowFunction) {
            const fnNode = varNode.initializer as ts.FunctionLikeDeclaration;
            const functionName = varNode.name as ts.Identifier;
            this.addExportFunction(functionName, fnNode);
          }
        } else if (ts.isFunctionLike(node)) {
          const fnNode = node as ts.FunctionLikeDeclaration;
          const functionName = fnNode.name as ts.Identifier;
          this.addExportFunction(functionName, fnNode);
        } else if (node.kind === ts.SyntaxKind.ExportSpecifier) {
          const exportNode = node as ts.ExportSpecifier;
          const functionName = exportNode.name as ts.Identifier;
          if (this.typeChecker.getTypeAtLocation(exportNode).symbol) {
            const innerNode = this.typeChecker.getTypeAtLocation(exportNode).symbol.declarations[0];
            if (ts.isFunctionLike(innerNode)) {
              this.addExportFunction(functionName, innerNode as ts.FunctionLikeDeclaration);
            }
          }
        }
      });

    // print result
    Object.values(this.interfaces).forEach(iface => {
      this.print(iface);
    });
    this.functions.forEach(fn => {
      this.print(fn);
    });
  }
}

const generator = new Generator(argv);
generator.do();
