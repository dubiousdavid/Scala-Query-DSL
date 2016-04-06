object QueryDSL {
  trait ColumnLike

  trait PreparedStatement {
    def toStatement: (String, Vector[Any])
  }

  type ColumnWithPrepared = ColumnLike with PreparedStatement

  case class Column(columnName: String) {
    private def withColumn(operator: String, column: Column) = ColumnStatement(
      s"$columnName $operator ${column.columnName}", Vector(), Vector()
    )
    private def withValue(operator: String, value: Any) = ColumnStatement(
      s"$columnName $operator ?", Vector(value), Vector()
    )

    def eql(column: Column): ColumnStatement = withColumn("=", column)
    def eql(value: Any): ColumnStatement = withValue("=", value)

    def notEql(column: Column): ColumnStatement = withColumn("<>", column)
    def notEql(value: Any): ColumnStatement = withValue("<>", value)

    def gt(column: Column): ColumnStatement = withColumn(">", column)
    def gt(value: Any): ColumnStatement = withValue(">", value)

    def gte(column: Column): ColumnStatement = withColumn(">=", column)
    def gte(value: Any): ColumnStatement = withValue(">=", value)

    def lt(column: Column): ColumnStatement = withColumn("<", column)
    def lt(value: Any): ColumnStatement = withValue("<", value)

    def lte(column: Column): ColumnStatement = withColumn("<=", column)
    def lte(value: Any): ColumnStatement = withValue("<=", value)

    def asc: ColumnStatement = ColumnStatement(s"$columnName ASC", Vector(), Vector())
    def desc: ColumnStatement = ColumnStatement(s"$columnName DESC", Vector(), Vector())

    def like(value: String): ColumnStatement = withValue("LIKE", value)

    def in(select: Select): ColumnStatement = {
      val (selectSql, selectPlaceHolders) = select.toStatement

      ColumnStatement(
        s"$columnName IN(\n$selectSql\n)", selectPlaceHolders, Vector()
      )
    }
    def in(values: Any*): ColumnStatement = ColumnStatement(
      s"$columnName IN(" + values.map(_ => "?").mkString(", ") + ")", values.toVector, Vector()
    )
  }

  case class ColumnStatement(
    sql: String,
    placeHolders: Vector[Any],
    subStatements: Vector[(String, ColumnWithPrepared)]
  ) extends ColumnLike with PreparedStatement {

    def toStatement = {
      val combinedSql = "(" + sql + subStatements.foldLeft("") {
        case (acc, (operator, statement)) => s"$acc $operator ${statement.toStatement._1}"
      } + ")"

      val subPlaceHolders = for {
        (_, statement) <- subStatements
        (_, statementPlaceHolders) = statement.toStatement
      } yield statementPlaceHolders

      (combinedSql, placeHolders.toVector ++ subPlaceHolders.flatten)
    }

    def and(subStatement: ColumnWithPrepared): ColumnStatement = this.copy(
      subStatements = subStatements :+ Tuple2("AND", subStatement)
    )

    def or(subStatement: ColumnWithPrepared): ColumnStatement = this.copy(
      subStatements = subStatements :+ Tuple2("OR", subStatement)
    )
  }

  case class Block(
    statement: ColumnWithPrepared,
    subStatements: Vector[(String, ColumnWithPrepared)]
  ) extends ColumnLike with PreparedStatement {

    def toStatement = {
      val (statementSql, statementPlaceHolders) = statement.toStatement

      val combinedSql = "(" + statementSql + subStatements.foldLeft("") {
        case (acc, (operator, statement)) => s"$acc $operator ${statement.toStatement._1}"
      } + ")"

      val subPlaceHolders = for {
        (_, statement) <- subStatements
        (_, statementPlaceHolders) = statement.toStatement
      } yield statementPlaceHolders

      (combinedSql, statementPlaceHolders ++ subPlaceHolders.flatten)
    }

    def and(subStatement: ColumnWithPrepared): Block = this.copy(
      subStatements = subStatements :+ Tuple2("AND", subStatement)
    )

    def or(subStatement: ColumnWithPrepared): Block = this.copy(
      subStatements = subStatements :+ Tuple2("OR", subStatement)
    )
  }

  trait Join extends PreparedStatement

  case class LeftJoin(tableName: String, on: ColumnWithPrepared) extends Join {
    def toStatement = {
      val (onSql, onPlaceHolders) = on.toStatement
      (s"LEFT JOIN $tableName ON $onSql", onPlaceHolders)
    }
  }

  case class RightJoin(tableName: String, on: ColumnWithPrepared) extends Join {
    def toStatement = {
      val (onSql, onPlaceHolders) = on.toStatement
      (s"RIGHT JOIN $tableName ON $onSql", onPlaceHolders)
    }
  }

  case class InnerJoin(tableName: String, on: ColumnWithPrepared) extends Join {
    def toStatement = {
      val (onSql, onPlaceHolders) = on.toStatement
      (s"INNER JOIN $tableName ON $onSql", onPlaceHolders)
    }
  }

  case class IncompleteSelect(columnNames: Seq[String]) extends PreparedStatement {
    def toStatement: Nothing = throw new Exception("Incomplete Select Statement")
    def from(tableName: String): Select = Select(columnNames, tableName)
  }

  case class Select(
    columnNames: Seq[String],
    tableName: String,
    where: Vector[ColumnWithPrepared],
    joins: Vector[Join],
    groupBys: Vector[ColumnWithPrepared]
  ) extends PreparedStatement {

    def from(tableName: String): Select = this.copy(tableName = tableName)
    def where(column: ColumnWithPrepared): Select = this.copy(where = where :+ column)

    def leftJoin(tableName: String, on: ColumnWithPrepared): Select = {
      this.copy(joins = joins :+ LeftJoin(tableName, on))
    }

    def rightJoin(tableName: String, on: ColumnWithPrepared): Select = {
      this.copy(joins = joins :+ RightJoin(tableName, on))
    }

    def innerJoin(tableName: String, on: ColumnWithPrepared): Select = {
      this.copy(joins = joins :+ InnerJoin(tableName, on))
    }

    def groupBy(column: ColumnWithPrepared): Select = this.copy(groupBys = groupBys :+ column)

    def toStatement = {
      // Where clauses
      val (whereClauses, wherePlaceHolders) =
        if (where.isEmpty) ("", Vector())
        else {
          val (clauses, placeHolders) = where.map(_.toStatement).unzip
          val sql = "WHERE " + clauses.mkString(" AND ")
          (sql, placeHolders)
        }

      // Join clauses
      val (joinClauses, joinPlaceHolders) = joins.map(_.toStatement).unzip

      // Group by clauses
      val (groupByClauses, groupByPlaceHolders) =
        if (groupBys.isEmpty) ("", Vector())
        else {
          val (clauses, placeHolders) = groupBys.map(_.toStatement).unzip
          val sql = "GROUP BY " + clauses.mkString(" AND ")
          (sql, placeHolders)
        }

      val placeHolders = joinPlaceHolders.flatten ++ wherePlaceHolders.flatten ++ groupByPlaceHolders.flatten

      val sql = Vector(
        s"SELECT ${columnNames.mkString(", ")}",
        s"FROM $tableName",
        joinClauses.mkString("\n"),
        whereClauses,
        groupByClauses
      ).filter(_.length > 0).mkString("\n")

      (sql, placeHolders)
    }
  }

  object Select {
    def apply(columnNames: Seq[String], tableName: String) =
      new Select(columnNames, tableName, Vector(), Vector(), Vector())
  }

  def select(columnNames: String*) = IncompleteSelect(columnNames)
  def col(columnName: String) = Column(columnName)
  def block(statement: ColumnWithPrepared): Block = Block(statement, Vector())

  // Example query
  val query =
    select("members.*", "someTable.column")
      .from("members")
      .leftJoin(
        "someTable",
        col("id").eql(col("someTable.id"))
          .and(col("someTable.id").notEql(3))
      )
      .innerJoin("anotherTable", col("id").eql(col("anotherTable.id")))
      .where(
        block(
          col("id").eql(2)
            .and(
              col("name").like("Bob%")
            )
            .and(col("age").gte(30))
        ).or(col("id").eql(92))
          .or(col("otherId").in(4, 5, 6))
          .and(col("field").in(
            select("aTable.field")
              .from("aTable")
              .where(col("field").eql("active"))
          ))
      )
      .groupBy(col("name").asc
        .and(
          col("id").desc
        ))
}
