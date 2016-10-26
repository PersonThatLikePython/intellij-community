/*
 * Copyright 2000-2014 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.intellij.codeInspection.dataFlow;

import com.intellij.codeInsight.NullableNotNullManager;
import com.intellij.codeInspection.dataFlow.MethodContract.ValueConstraint;
import com.intellij.lang.LighterAST;
import com.intellij.lang.LighterASTNode;
import com.intellij.lang.TreeBackedLighterAST;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.util.RecursionManager;
import com.intellij.psi.*;
import com.intellij.psi.impl.source.JavaLightTreeUtil;
import com.intellij.psi.impl.source.tree.ElementType;
import com.intellij.psi.impl.source.tree.LightTreeUtil;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;
import com.intellij.psi.util.PsiModificationTracker;
import com.intellij.psi.util.PsiUtil;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static com.intellij.codeInspection.dataFlow.MethodContract.ValueConstraint.*;
import static com.intellij.psi.impl.source.JavaLightTreeUtil.findExpressionChild;
import static com.intellij.psi.impl.source.JavaLightTreeUtil.getExpressionChildren;
import static com.intellij.psi.impl.source.tree.JavaElementType.*;
import static com.intellij.psi.impl.source.tree.LightTreeUtil.firstChildOfType;

/**
 * @author peter
 */
public class ContractInference {
  private static final Logger LOG = Logger.getInstance("#com.intellij.codeInspection.dataFlow.ContractInferenceInterpreter");
  public static final int MAX_CONTRACT_COUNT = 10;

  @NotNull
  public static List<MethodContract> inferContracts(@NotNull final PsiMethod method) {
    if (!InferenceFromSourceUtil.shouldInferFromSource(method)) {
      return Collections.emptyList();
    }

    return CachedValuesManager.getCachedValue(method, () -> {
      TreeBackedLighterAST tree = new TreeBackedLighterAST(method.getContainingFile().getNode());
      PsiCodeBlock body = method.getBody();
      assert body != null;
      List<PreContract> preContracts = new ContractInferenceInterpreter(tree, method, TreeBackedLighterAST.wrap(body.getNode())).inferContracts();
      List<MethodContract> result = RecursionManager.doPreventingRecursion(method, true, () -> postProcessContracts(method, body, preContracts));
      if (result == null) result = Collections.emptyList();
      return CachedValueProvider.Result.create(result, method, PsiModificationTracker.JAVA_STRUCTURE_MODIFICATION_COUNT);
    });
  }

  @NotNull
  private static List<MethodContract> postProcessContracts(@NotNull PsiMethod method, @NotNull PsiCodeBlock body, List<PreContract> rawContracts) {
    List<MethodContract> contracts = ContainerUtil.concat(rawContracts, c -> c.toContracts(method, body));
    if (contracts.isEmpty()) return Collections.emptyList();
    
    final PsiType returnType = method.getReturnType();
    if (returnType != null && !(returnType instanceof PsiPrimitiveType)) {
      contracts = boxReturnValues(contracts);
    }
    List<MethodContract> compatible = ContainerUtil.filter(contracts, contract -> {
      if ((contract.returnValue == NOT_NULL_VALUE || contract.returnValue == NULL_VALUE) &&
          NullableNotNullManager.getInstance(method.getProject()).isNotNull(method, false)) {
        return false;
      }
      return InferenceFromSourceUtil.isReturnTypeCompatible(returnType, contract.returnValue);
    });
    if (compatible.size() > MAX_CONTRACT_COUNT) {
      LOG.debug("Too many contracts for " + PsiUtil.getMemberQualifiedName(method) + ", shrinking the list");
      return compatible.subList(0, MAX_CONTRACT_COUNT);
    }
    return compatible;
  }

  @NotNull
  private static List<MethodContract> boxReturnValues(List<MethodContract> contracts) {
    return ContainerUtil.mapNotNull(contracts, contract -> {
      if (contract.returnValue == FALSE_VALUE || contract.returnValue == TRUE_VALUE) {
        return new MethodContract(contract.arguments, NOT_NULL_VALUE);
      }
      return contract;
    });
  }

}

class ContractInferenceInterpreter {
  private final LighterAST myTree;
  private final PsiMethod myMethod;
  private final ValueConstraint[] myEmptyConstraints;
  private final LighterASTNode myBody;

  public ContractInferenceInterpreter(LighterAST tree, PsiMethod method, LighterASTNode body) {
    myTree = tree;
    myMethod = method;
    myBody = body;
    myEmptyConstraints = MethodContract.createConstraintArray(myMethod.getParameterList().getParametersCount());
  }

  List<PreContract> inferContracts() {
    LighterASTNode[] statements = getStatements(myBody);
    if (statements.length == 0) return Collections.emptyList();

    if (statements.length == 1) {
      if (statements[0].getTokenType() == RETURN_STATEMENT) {
        List<PreContract> result = handleDelegation(findExpressionChild(myTree, statements[0]), false);
        if (result != null) {
          return result;
        }
      }
      else if (statements[0].getTokenType() == EXPRESSION_STATEMENT) {
        LighterASTNode expr = findExpressionChild(myTree, statements[0]);
        List<PreContract> result = expr != null && expr.getTokenType() == METHOD_CALL_EXPRESSION ? handleDelegation(expr, false) : null;
        if (result != null) return result;
      }
    }

    return visitStatements(Collections.singletonList(myEmptyConstraints), statements);
  }

  @Nullable
  private LighterASTNode getCodeBlock(@Nullable LighterASTNode parent) {
    return firstChildOfType(myTree, parent, CODE_BLOCK);
  }

  @NotNull
  private LighterASTNode[] getStatements(@Nullable LighterASTNode codeBlock) {
    return codeBlock == null
           ? LighterASTNode.EMPTY_ARRAY
           : LightTreeUtil.getChildrenOfType(myTree, codeBlock, ElementType.JAVA_STATEMENT_BIT_SET).toArray(LighterASTNode.EMPTY_ARRAY);
  }

  @Nullable
  private List<PreContract> handleDelegation(@Nullable LighterASTNode expression, boolean negated) {
    if (expression == null) return null;
    if (expression.getTokenType() == PARENTH_EXPRESSION) {
      return handleDelegation(findExpressionChild(myTree, expression), negated);
    }

    if (isNegationExpression(expression)) {
      return handleDelegation(findExpressionChild(myTree, expression), !negated);
    }

    if (expression.getTokenType() == METHOD_CALL_EXPRESSION) {
      return Collections.singletonList(new DelegationContract(ExpressionRange.create(expression, myBody.getStartOffset()), negated));
    }

    return null;
  }

  private boolean isNegationExpression(@Nullable LighterASTNode expression) {
    return expression != null && expression.getTokenType() == PREFIX_EXPRESSION && firstChildOfType(myTree, expression, JavaTokenType.EXCL) != null;
  }

  @NotNull
  private List<PreContract> visitExpression(final List<ValueConstraint[]> states, @Nullable LighterASTNode expr) {
    if (expr == null) return Collections.emptyList();
    if (states.isEmpty()) return Collections.emptyList();
    if (states.size() > 300) return Collections.emptyList(); // too complex

    IElementType type = expr.getTokenType();
    if (type == POLYADIC_EXPRESSION || type == BINARY_EXPRESSION) {
      List<LighterASTNode> operands = getExpressionChildren(myTree, expr);
      if (operands.size() == 2) {
        boolean equality = firstChildOfType(myTree, expr, JavaTokenType.EQEQ) != null;
        if (equality || firstChildOfType(myTree, expr, JavaTokenType.NE) != null) {
          return asPreContracts(visitEqualityComparison(states, operands.get(0), operands.get(1), equality));
        }
      }
      boolean logicalAnd = firstChildOfType(myTree, expr, JavaTokenType.ANDAND) != null;
      if (logicalAnd || firstChildOfType(myTree, expr, JavaTokenType.OROR) != null) {
        return asPreContracts(visitLogicalOperation(operands, logicalAnd, states));
      }
    }

    if (type == CONDITIONAL_EXPRESSION) {
      List<LighterASTNode> children = getExpressionChildren(myTree, expr);
      if (children.size() != 3) return Collections.emptyList();

      List<PreContract> conditionResults = visitExpression(states, children.get(0));
      return ContainerUtil.concat(
        visitExpression(antecedentsReturning(conditionResults, TRUE_VALUE), children.get(1)),
        visitExpression(antecedentsReturning(conditionResults, FALSE_VALUE), children.get(2)));
    }


    if (type == PARENTH_EXPRESSION) {
      return visitExpression(states, findExpressionChild(myTree, expr));
    }
    if (type == TYPE_CAST_EXPRESSION) {
      return visitExpression(states, findExpressionChild(myTree, expr));
    }

    if (isNegationExpression(expr)) {
      List<PreContract> result = ContainerUtil.newArrayList();
      for (PreContract contract : visitExpression(states, findExpressionChild(myTree, expr))) {
        ContainerUtil.addIfNotNull(result, NegatingContract.negate(contract));
      }
      return result;
    }

    if (type == INSTANCE_OF_EXPRESSION) {
      final int parameter = resolveParameter(findExpressionChild(myTree, expr));
      if (parameter >= 0) {
        return asPreContracts(ContainerUtil.mapNotNull(states, state -> contractWithConstraint(state, parameter, NULL_VALUE, FALSE_VALUE)));
      }
    }

    if (type == NEW_EXPRESSION) {
      return asPreContracts(toContracts(states, NOT_NULL_VALUE));
    }
    if (type == METHOD_CALL_EXPRESSION) {
      return Collections.singletonList(new MethodCallContract(ExpressionRange.create(expr, myBody.getStartOffset()), states));
    }

    final ValueConstraint constraint = getLiteralConstraint(expr);
    if (constraint != null) {
      return asPreContracts(toContracts(states, constraint));
    }

    int paramIndex = resolveParameter(expr);
    if (paramIndex >= 0) {
      List<MethodContract> result = ContainerUtil.newArrayList();
      for (ValueConstraint[] state : states) {
        if (state[paramIndex] != ANY_VALUE) {
          // the second 'o' reference in cases like: if (o != null) return o;
          result.add(new MethodContract(state, state[paramIndex]));
        } else if (textMatches(getParameter(paramIndex).getTypeElement(), PsiKeyword.BOOLEAN)) {
          // if (boolValue) ...
          ContainerUtil.addIfNotNull(result, contractWithConstraint(state, paramIndex, TRUE_VALUE, TRUE_VALUE));
          ContainerUtil.addIfNotNull(result, contractWithConstraint(state, paramIndex, FALSE_VALUE, FALSE_VALUE));
        }
      }
      return asPreContracts(result);
    }

    return Collections.emptyList();
  }

  @NotNull
  private static List<PreContract> asPreContracts(List<MethodContract> contracts) {
    return ContainerUtil.map(contracts, KnownContract::new);
  }

  @Nullable
  private MethodContract contractWithConstraint(ValueConstraint[] state,
                                                       int parameter, ValueConstraint paramConstraint,
                                                       ValueConstraint returnValue) {
    ValueConstraint[] newState = withConstraint(state, parameter, paramConstraint, myMethod);
    return newState == null ? null : new MethodContract(newState, returnValue);
  }

  private static boolean textMatches(@Nullable PsiTypeElement typeElement, @NotNull String text) {
    return typeElement != null && typeElement.textMatches(text);
  }

  private List<MethodContract> visitEqualityComparison(List<ValueConstraint[]> states,
                                                       LighterASTNode op1,
                                                       LighterASTNode op2,
                                                       boolean equality) {
    int parameter = resolveParameter(op1);
    ValueConstraint constraint = getLiteralConstraint(op2);
    if (parameter < 0 || constraint == null) {
      parameter = resolveParameter(op2);
      constraint = getLiteralConstraint(op1);
    }
    if (parameter >= 0 && constraint != null) {
      List<MethodContract> result = ContainerUtil.newArrayList();
      for (ValueConstraint[] state : states) {
        if (constraint == NOT_NULL_VALUE) {
          if (!(getParameter(parameter).getType() instanceof PsiPrimitiveType)) {
            ContainerUtil.addIfNotNull(result, contractWithConstraint(state, parameter, NULL_VALUE, equality ? FALSE_VALUE : TRUE_VALUE));
          }
        } else {
          ContainerUtil.addIfNotNull(result, contractWithConstraint(state, parameter, constraint, equality ? TRUE_VALUE : FALSE_VALUE));
          ContainerUtil.addIfNotNull(result, contractWithConstraint(state, parameter, negateConstraint(constraint),
                                                                    equality ? FALSE_VALUE : TRUE_VALUE));
        }
      }
      return result;
    }
    return Collections.emptyList();
  }

  private PsiParameter getParameter(int parameter) {
    return getParameter(parameter, myMethod);
  }

  private static PsiParameter getParameter(int parameter, PsiMethod method) {
    return method.getParameterList().getParameters()[parameter];
  }

  static List<MethodContract> toContracts(List<ValueConstraint[]> states, ValueConstraint constraint) {
    return ContainerUtil.map(states, state -> new MethodContract(state, constraint));
  }

  private List<MethodContract> visitLogicalOperation(List<LighterASTNode> operands, boolean conjunction, List<ValueConstraint[]> states) {
    ValueConstraint breakValue = conjunction ? FALSE_VALUE : TRUE_VALUE;
    List<MethodContract> finalStates = ContainerUtil.newArrayList();
    for (LighterASTNode operand : operands) {
      List<PreContract> opResults = visitExpression(states, operand);
      finalStates.addAll(ContainerUtil.filter(knownContracts(opResults), contract -> contract.returnValue == breakValue));
      states = antecedentsReturning(opResults, negateConstraint(breakValue));
    }
    finalStates.addAll(toContracts(states, negateConstraint(breakValue)));
    return finalStates;
  }

  private static List<MethodContract> knownContracts(List<PreContract> values) {
    return ContainerUtil.mapNotNull(values, pc -> pc instanceof KnownContract ? ((KnownContract)pc).getContract() : null);
  }

  private static List<ValueConstraint[]> antecedentsReturning(List<PreContract> values, ValueConstraint result) {
    return ContainerUtil.mapNotNull(knownContracts(values), contract -> contract.returnValue == result ? contract.arguments : null);
  }

  private static class CodeBlockContracts {
    List<PreContract> accumulated = new ArrayList<>();
    List<ExpressionRange> varInitializers = new ArrayList<>();

    void addAll(List<PreContract> contracts) {
      if (contracts.isEmpty()) return;

      if (varInitializers.isEmpty()) {
        accumulated.addAll(contracts);
      } else {
        accumulated.add(new SideEffectFilter(varInitializers, contracts));
      }
    }

    void registerDeclaration(@NotNull LighterASTNode declStatement, @NotNull LighterAST tree, int scopeStart) {
      for (LighterASTNode var : LightTreeUtil.getChildrenOfType(tree, declStatement, LOCAL_VARIABLE)) {
        LighterASTNode initializer = findExpressionChild(tree, var);
        if (initializer != null) {
          varInitializers.add(ExpressionRange.create(initializer, scopeStart));
        }
      }
    }
  }

  @NotNull
  private List<PreContract> visitStatements(List<ValueConstraint[]> states, LighterASTNode... statements) {
    CodeBlockContracts result = new CodeBlockContracts();
    for (LighterASTNode statement : statements) {
      IElementType type = statement.getTokenType();
      if (type == BLOCK_STATEMENT) {
        result.addAll(visitStatements(states, getStatements(getCodeBlock(statement))));
      }
      else if (type == IF_STATEMENT) {
        List<PreContract> conditionResults = visitExpression(states, findExpressionChild(myTree, statement));

        LighterASTNode[] thenElse = getStatements(statement);
        if (thenElse.length > 0) {
          result.addAll(visitStatements(antecedentsReturning(conditionResults, TRUE_VALUE), thenElse[0]));
        }

        List<ValueConstraint[]> falseStates = antecedentsReturning(conditionResults, FALSE_VALUE);
        if (thenElse.length > 1) {
          result.addAll(visitStatements(falseStates, thenElse[1]));
        } else {
          states = falseStates;
          continue;
        }
      }
      else if (type == WHILE_STATEMENT) {
        states = antecedentsReturning(visitExpression(states, findExpressionChild(myTree, statement)), FALSE_VALUE);
        continue;
      }
      else if (type == THROW_STATEMENT) {
        result.addAll(asPreContracts(toContracts(states, THROW_EXCEPTION)));
      }
      else if (type == RETURN_STATEMENT) {
        result.addAll(visitExpression(states, findExpressionChild(myTree, statement)));
      }
      else if (type == ASSERT_STATEMENT) {
        List<PreContract> conditionResults = visitExpression(states, findExpressionChild(myTree, statement));
        result.addAll(asPreContracts(toContracts(antecedentsReturning(conditionResults, FALSE_VALUE), THROW_EXCEPTION)));
      }
      else if (type == DECLARATION_STATEMENT) {
        result.registerDeclaration(statement, myTree, myBody.getStartOffset());
        continue;
      }
      else if (type == DO_WHILE_STATEMENT) {
        result.addAll(visitStatements(states, getStatements(statement)));
      }

      break; // visit only the first statement unless it's 'if' whose 'then' always returns and the next statement is effectively 'else'
    }
    return result.accumulated;
  }

  @Nullable
  private ValueConstraint getLiteralConstraint(@Nullable LighterASTNode expr) {
    if (expr != null && expr.getTokenType() == LITERAL_EXPRESSION) {
      return getLiteralConstraint(myTree.getChildren(expr).get(0).getTokenType());
    }
    return null;
  }

  @NotNull
  static ValueConstraint getLiteralConstraint(@NotNull IElementType literalTokenType) {
    if (literalTokenType.equals(JavaTokenType.TRUE_KEYWORD)) return TRUE_VALUE;
    if (literalTokenType.equals(JavaTokenType.FALSE_KEYWORD)) return FALSE_VALUE;
    if (literalTokenType.equals(JavaTokenType.NULL_KEYWORD)) return NULL_VALUE;
    return NOT_NULL_VALUE;
  }

  static ValueConstraint negateConstraint(@NotNull ValueConstraint constraint) {
    //noinspection EnumSwitchStatementWhichMissesCases
    switch (constraint) {
      case NULL_VALUE: return NOT_NULL_VALUE;
      case NOT_NULL_VALUE: return NULL_VALUE;
      case TRUE_VALUE: return FALSE_VALUE;
      case FALSE_VALUE: return TRUE_VALUE;
    }
    return constraint;
  }

  private int resolveParameter(@Nullable LighterASTNode expr) {
    if (expr != null && expr.getTokenType() == REFERENCE_EXPRESSION && findExpressionChild(myTree, expr) == null) {
      String name = JavaLightTreeUtil.getNameIdentifierText(myTree, expr);
      if (name == null) return -1;

      PsiParameter[] parameters = myMethod.getParameterList().getParameters();
      for (int i = 0; i < parameters.length; i++) {
        if (name.equals(parameters[i].getName())) {
          return i;
        }
      }
    }
    return -1;
  }

  @Nullable
  static ValueConstraint[] withConstraint(ValueConstraint[] constraints, int index, ValueConstraint constraint, PsiMethod method) {
    if (constraints[index] == constraint) return constraints;

    ValueConstraint negated = negateConstraint(constraint);
    if (negated != constraint && constraints[index] == negated) {
      return null;
    }

    if (constraint == NULL_VALUE && NullableNotNullManager.isNotNull(getParameter(index, method))) {
      return null;
    }

    ValueConstraint[] copy = constraints.clone();
    copy[index] = constraint;
    return copy;
  }

}