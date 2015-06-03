/*
 * Copyright 2000-2015 JetBrains s.r.o.
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

/*
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 28, 2002
 * Time: 10:05:49 PM
 * To change template for new class use 
 * Code Style | Class Templates options (Tools | IDE Options).
 */
package com.intellij.codeInspection.dataFlow.instructions;

import com.intellij.codeInspection.dataFlow.DfaInstructionState;
import com.intellij.codeInspection.dataFlow.DfaMemoryState;
import com.intellij.codeInspection.dataFlow.InstructionVisitor;
import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class ReturnInstruction extends Instruction {
  private final boolean isViaException;
  private final PsiElement myAnchor;

  public ReturnInstruction(boolean isViaException, @Nullable PsiElement anchor) {
    this.isViaException = isViaException;
    myAnchor = anchor;
  }

  @Nullable
  public PsiElement getAnchor() {
    return myAnchor;
  }

  public boolean isViaException() {
    return isViaException;
  }

  @Override
  public DfaInstructionState[] accept(@NotNull DfaMemoryState stateBefore, @NotNull InstructionVisitor visitor) {
    return DfaInstructionState.EMPTY_ARRAY;
  }

  public String toString() {
    return "RETURN";
  }
}
