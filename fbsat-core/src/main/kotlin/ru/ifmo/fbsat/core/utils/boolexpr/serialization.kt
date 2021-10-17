package ru.ifmo.fbsat.core.utils.boolexpr

import kotlinx.serialization.modules.SerializersModule
import kotlinx.serialization.modules.polymorphic
import kotlinx.serialization.modules.subclass

@Suppress("PublicApiImplicitType")
val booleanExpressionModule = SerializersModule {
    polymorphic(BooleanExpression::class) {
        subclass(Constant::class)
        subclass(Variable::class)
        subclass(UnaryOperation::class)
        subclass(BinaryOperation::class)
    }
}
