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
package com.intellij.codeInspection.dataFlow.instructions;

import com.intellij.codeInspection.dataFlow.DfaInstructionState;
import com.intellij.codeInspection.dataFlow.DfaMemoryState;
import com.intellij.codeInspection.dataFlow.InstructionVisitor;
import org.jetbrains.annotations.NotNull;

/**
 * @author max
 */
public class DupInstruction extends Instruction {
  private final int myValueCount;
  private final int myDuplicationCount;

  public DupInstruction() {
    this(1, 1);
  }

  public DupInstruction(int valueCount, int duplicationCount) {
    myValueCount = valueCount;
    myDuplicationCount = duplicationCount;
  }

  public int getValueCount() {
    return myValueCount;
  }

  public int getDuplicationCount() {
    return myDuplicationCount;
  }

  @Override
  public DfaInstructionState[] accept(@NotNull DfaMemoryState memState, @NotNull InstructionVisitor visitor) {
    return visitor.visitDuplicate(this, memState);
  }

  public String toString() {
    return "DUP(" + myValueCount + " top stack values, " + myDuplicationCount + " times)";
  }
}
