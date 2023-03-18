grammar PGMFile;

/*
    PGM File specification

    TODO(s5khatta) allow for variables/constants?
 */

pgm : header data;
header : pgmName pgmType;
pgmName : IDENTIFIER;
pgmType : KW_BAYESIAN # bayesianPGMType
        | KW_MARKOV   # markovPGMType
        ;

data : secConnections secNodeDefinitions secQueries;

/*
    CONNECTIONS SECTION
*/
secConnections : KW_CONNECTIONS ':' connections;
connections : connection +;

connection : node connectionTo+; // TODO(s5khatta) allow longer lines (-+>?) ?
connectionTo : op=('->' | '-') node;

/*
    NODE DEFINITION SECTION
*/

secNodeDefinitions : KW_NODE_DEFINITIONS ':' nodeDefinitions;
nodeDefinitions : nodeDefinition +;

nodeDefinition : node ('<' parentList '>')? ':' '[' valuesList ']' '{' cpd '}';
parentList : node (',' node)*;
valuesList : nodeValue (',' nodeValue)*;
cpd : cpdRow ';'?
    | cpdRow (';' cpdRow) + ';'?;
cpdRow : NUMBER (',' NUMBER)*;

/*
    QUERIES SECTION
*/

secQueries : KW_QUERIES ':' queries;
queries : query +;
query : marginalQuery
    | expectationQuery
    | mapQuery
    ;

marginalQuery      : 'P'   '[' queryStatementList '|' evidenceList ']' queryName epsilonDeltaSpecification;
expectationQuery   : 'E'   '[' queryStatement ('|' evidenceList)? ']' queryName epsilonDeltaSpecification;
mapQuery           : 'MAP' '[' queryStatement '|' evidenceList ']' queryName epsilonDeltaSpecification;
queryStatementList: queryStatement (',' queryStatement)*;
queryStatement    : queryExpression             # qsExpression
                  | relationQueryExpression     # qsRelation
                  ;

relationQueryExpression : queryExpression '=' queryExpression   # rqeEquals
                        | queryExpression '<=' queryExpression  # rqeLessThanEquals
                        | queryExpression '>' queryExpression   # rqeGreaterThan
                        | queryExpression '>=' queryExpression  # rqeGreaterThanEquals
                        ;

queryExpression : '(' queryExpression ')'               # qeParens
                | queryExpression '^' queryExpression     # qePow
                | queryExpression '*' queryExpression     # qeMultiply
                | queryExpression '/' queryExpression     # qeDivide
                | queryExpression '+' queryExpression     # qePlus
                | queryExpression '-' queryExpression     # qeMinus
                | node                               # qeNode
                | NUMBER                                # qeValue
                ;

evidenceList       : evidence (',' evidence)*;
evidence: node               # evidenceAtRuntime
        | node '=' nodeValue # evidenceAtCompileTime
        ;

queryName : '#' IDENTIFIER;
epsilonDeltaSpecification : '<' epsilonSpecifier ',' deltaSpecifier '>';
epsilonSpecifier : NUMBER;
deltaSpecifier : NUMBER;

/*
    Reused rules
*/
node : IDENTIFIER;
nodeValue : NUMBER; // TODO(s5khatta) decide whether to allow real numbers

/*
    Tokens
*/
KW_QUERIES          : 'QUERIES'     | 'queries';
KW_NODE_DEFINITIONS : 'NODES'       | 'nodes';
KW_CONNECTIONS      : 'CONNECTIONS' | 'connections';
KW_BAYESIAN         : 'BAYESIAN'    | 'bayesian';
KW_MARKOV           : 'MARKOV'      | 'markov';

KW_DIRECTED_EDGE : '->';
KW_UNDIRECTED_EDGE : '-';

IDENTIFIER : [a-zA-Z] [a-zA-Z0-9_]*;
NUMBER : INT
    | PROBABILITY;
INT : [0-9]+;
PROBABILITY : [0-9]* '.'? [0-9]+;   // TODO(s5khatta) enforce semantic correctness here?
STRING : '"' .*? '"'; // TODO(s5khatta) enforce semantic correctness here?

// skipped tokens
WS  :   [ \t\r\n]+                  -> skip ; // Define whitespace rule, toss it out
LINE_COMMENT : '//' .*? '\r'? '\n'  -> skip ; // Match "//" stuff '\n'
COMMENT : '/*' .*? '*/'             -> skip ; // Match "/*" stuff "*/"
