/*
 * Copyright 2000-2017 JetBrains s.r.o.
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

package com.intellij.codeInspection.dataFlow.instructions;

import com.intellij.codeInspection.dataFlow.DataFlowRunner;
import com.intellij.codeInspection.dataFlow.DfaInstructionState;
import com.intellij.codeInspection.dataFlow.DfaMemoryState;
import com.intellij.codeInspection.dataFlow.InstructionVisitor;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiType;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import org.jetbrains.annotations.Nullable;

import static com.intellij.psi.JavaTokenType.*;

public class BinopInstruction extends BranchingInstruction {
  private static final TokenSet ourSignificantOperations =
    TokenSet.create(EQEQ, NE, LT, GT, LE, GE, INSTANCEOF_KEYWORD, PLUS, MINUS, AND, PERC, DIV, GTGT, GTGTGT);
  private final IElementType myOperationSign;
  private final @Nullable PsiType myResultType;

  public BinopInstruction(IElementType opSign, @Nullable PsiElement psiAnchor, @Nullable PsiType resultType) {
    super(psiAnchor);
    myResultType = resultType;
    myOperationSign = ourSignificantOperations.contains(opSign) ? opSign : null;
  }

  @Override
  public DfaInstructionState[] accept(DataFlowRunner runner, DfaMemoryState stateBefore, InstructionVisitor visitor) {
    return visitor.visitBinop(this, runner, stateBefore);
  }

  @Nullable
  public PsiType getResultType() {
    return myResultType;
  }

  public String toString() {
    return "BINOP " + myOperationSign;
  }

  public IElementType getOperationSign() {
    return myOperationSign;
  }
}
