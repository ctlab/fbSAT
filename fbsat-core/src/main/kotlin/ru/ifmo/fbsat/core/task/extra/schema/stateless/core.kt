package ru.ifmo.fbsat.core.task.extra.schema.stateless



private var iSchemaBlockType = 0

enum class SchemaBlockType(val value: Int) {
    AND(++iSchemaBlockType),
    OR(++iSchemaBlockType),
    NOT(++iSchemaBlockType),
    ;

    companion object {
        private val lookup: Map<Int, SchemaBlockType> = values().associateBy(SchemaBlockType::value)

        fun from(value: Int): SchemaBlockType = lookup.getValue(value)
    }
}
