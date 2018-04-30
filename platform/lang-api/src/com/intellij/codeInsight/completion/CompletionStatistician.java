/*
 * Copyright 2000-2009 JetBrains s.r.o.
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
package com.intellij.codeInsight.completion;

import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.psi.statistics.Statistician;
import com.intellij.psi.statistics.StatisticsInfo;
import org.jetbrains.annotations.NotNull;

/**
 * A {@link Statistician} for code completion, the results are used for sorting and preselection.
 */
public abstract class CompletionStatistician extends Statistician<LookupElement,CompletionLocation> {
  @Override
  public abstract StatisticsInfo serialize(@NotNull final LookupElement element, @NotNull final CompletionLocation location);
}
