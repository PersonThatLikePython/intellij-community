// Copyright 2000-2022 JetBrains s.r.o. and contributors. Use of this source code is governed by the Apache 2.0 license.
package org.jetbrains.kotlin.idea.k2.codeinsight.inspections

import com.intellij.refactoring.RefactoringBundle
import com.intellij.refactoring.classMembers.MemberInfoBase
import org.jetbrains.kotlin.descriptors.Modality
import org.jetbrains.kotlin.idea.base.resources.KotlinBundle
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.psi.KtClass
import org.jetbrains.kotlin.psi.KtFile
import org.jetbrains.kotlin.psi.KtNamedDeclaration
import org.jetbrains.kotlin.psi.psiUtil.modalityModifierType

class K2KotlinMemberInfo @JvmOverloads constructor(
    member: KtNamedDeclaration,
    val isSuperClass: Boolean = false,
    val isCompanionMember: Boolean = false
) : MemberInfoBase<KtNamedDeclaration>(member) {
    init {
        isStatic = member.parent is KtFile

        if (member is KtClass && isSuperClass) {
            if (member.isInterface()) {
                displayName = RefactoringBundle.message("member.info.implements.0", member.name)
                overrides = false
            } else {
                displayName = RefactoringBundle.message("member.info.extends.0", member.name)
                overrides = true
            }
        } else {
            displayName = member.name
            if (member.hasModifier(KtTokens.ABSTRACT_KEYWORD)) {
                displayName = KotlinBundle.message("member.info.abstract.0", displayName)
            }
            if (isCompanionMember) {
                displayName = KotlinBundle.message("member.info.companion.0", displayName)
            }

            overrides = member.modalityModifierType() == KtTokens.ABSTRACT_KEYWORD
            val overriddenDescriptors = (memberDescriptor as? CallableMemberDescriptor)?.overriddenDescriptors ?: emptySet()
            if (overriddenDescriptors.isNotEmpty()) {
                overrides = overriddenDescriptors.any { it.modality != Modality.ABSTRACT }
            }
        }
    }
}