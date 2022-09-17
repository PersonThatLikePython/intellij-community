// Copyright 2000-2022 JetBrains s.r.o. and contributors. Use of this source code is governed by the Apache 2.0 license.
package org.jetbrains.kotlin.idea.k2.codeinsight.inspections

import com.intellij.codeInsight.generation.ui.AbstractGenerateEqualsWizard
import org.jetbrains.kotlin.psi.KtClass
import org.jetbrains.kotlin.psi.KtNamedDeclaration

class K2KotlinGenerateEqualsWizard : AbstractGenerateEqualsWizard<KtClass, KtNamedDeclaration, > {}