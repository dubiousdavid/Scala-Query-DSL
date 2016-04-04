object QueryDSL {
  trait Statement {
    def toStatement: (String, Vector[Any])
  }

  case class Column(columnName: String) {
    private def withColumn(operator: String, column: Column) = ColumnStatement(
      s"$columnName $operator ${column.columnName}", None, Vector()
    )
    private def withValue(operator: String, value: Any) = ColumnStatement(
      s"$columnName $operator ?", Some(value), Vector()
    )

    def eq(column: Column): ColumnStatement = withColumn("=", column)
    def eq(value: Any): ColumnStatement = withValue("=", value)

    def notEq(column: Column): ColumnStatement = withColumn("<>", column)
    def notEq(value: Any): ColumnStatement = withValue("<>", value)

    def gt(column: Column): ColumnStatement = withColumn(">", column)
    def gt(value: Any): ColumnStatement = withValue(">", value)

    def gte(column: Column): ColumnStatement = withColumn(">=", column)
    def gte(value: Any): ColumnStatement = withValue(">=", value)

    def lt(column: Column): ColumnStatement = withColumn("<", column)
    def lt(value: Any): ColumnStatement = withValue("<", value)

    def lte(column: Column): ColumnStatement = withColumn("<=", column)
    def lte(value: Any): ColumnStatement = withValue("<=", value)

    def asc: ColumnStatement = ColumnStatement(s"$columnName ASC", None, Vector())
    def desc: ColumnStatement = ColumnStatement(s"$columnName DESC", None, Vector())

    def like(value: String): ColumnStatement = withValue("LIKE", value)
  }

  case class ColumnStatement(
    sql: String,
    placeHolder: Option[Any],
    subStatements: Vector[(String, ColumnStatement)]
  ) extends Statement {

    def toStatement = {
      val combinedSql = sql + subStatements.foldLeft("") {
        case (acc, (operator, statement)) => s"$acc $operator ${statement.sql}"
      }

      val subPlaceHolders = for {
        (_, statement) <- subStatements
        placeHolder <- statement.placeHolder
      } yield placeHolder

      (combinedSql, placeHolder.toVector ++ subPlaceHolders)
    }

    def and(subStatement: ColumnStatement): ColumnStatement = this.copy(
      subStatements = subStatements :+ Tuple2("AND", subStatement)
    )

    def or(subStatement: ColumnStatement): ColumnStatement = this.copy(
      subStatements = subStatements :+ Tuple2("OR", subStatement)
    )
  }

  trait Join extends Statement

  case class LeftJoin(tableName: String, on: ColumnStatement) extends Join {
    def toStatement = {
      val (onSql, onPlaceHolders) = on.toStatement
      (s"LEFT JOIN $tableName ON $onSql", onPlaceHolders)
    }
  }

  case class RightJoin(tableName: String, on: ColumnStatement) extends Join {
    def toStatement = {
      val (onSql, onPlaceHolders) = on.toStatement
      (s"RIGHT JOIN $tableName ON $onSql", onPlaceHolders)
    }
  }

  case class InnerJoin(tableName: String, on: ColumnStatement) extends Join {
    def toStatement = {
      val (onSql, onPlaceHolders) = on.toStatement
      (s"INNER JOIN $tableName ON $onSql", onPlaceHolders)
    }
  }

  case class IncompleteSelect(columnNames: Seq[String]) extends Statement {
    def toStatement: Nothing = throw new Exception("Incomplete Select Statement")
    def from(tableName: String): Select = Select(columnNames, tableName)
  }

  case class Select(
    columnNames: Seq[String],
    tableName: String,
    where: Vector[ColumnStatement],
    joins: Vector[Join],
    groupBys: Vector[ColumnStatement]
  ) extends Statement {

    def from(tableName: String): Select = this.copy(tableName = tableName)
    def where(column: ColumnStatement): Select = this.copy(where = where :+ column)

    def leftJoin(tableName: String, on: ColumnStatement): Select = {
      this.copy(joins = joins :+ LeftJoin(tableName, on))
    }

    def rightJoin(tableName: String, on: ColumnStatement): Select = {
      this.copy(joins = joins :+ RightJoin(tableName, on))
    }

    def innerJoin(tableName: String, on: ColumnStatement): Select = {
      this.copy(joins = joins :+ InnerJoin(tableName, on))
    }

    def groupBy(column: ColumnStatement): Select = this.copy(groupBys = groupBys :+ column)

    def toStatement = {
      val (whereClauses, wherePlaceHolders) =
        if (where.isEmpty) ("", Vector())
        else {
          val (clauses, placeHolders) = where.map(_.toStatement).unzip
          val sql = "WHERE " + clauses.mkString(" AND ")
          (sql, placeHolders)
        }

      val (joinClauses, joinPlaceHolders) = joins.map(_.toStatement).unzip

      val (groupByClauses, groupByPlaceHolders) =
        if (groupBys.isEmpty) ("", Vector())
        else {
          val (clauses, placeHolders) = groupBys.map(_.toStatement).unzip
          val sql = "GROUP BY " + clauses.mkString(" AND ")
          (sql, placeHolders)
        }

      val placeHolders = joinPlaceHolders.flatten ++ wherePlaceHolders.flatten ++ groupByPlaceHolders.flatten

      val sql = s"""
      |SELECT ${columnNames.mkString(", ")}
      |FROM $tableName
      |${joinClauses.mkString("\n")}
      |$whereClauses
      |$groupByClauses
      """.stripMargin.trim

      (sql, placeHolders)
    }
  }

  object Select {
    def apply(columnNames: Seq[String], tableName: String) =
      new Select(columnNames, tableName, Vector(), Vector(), Vector())
  }

  def select(columnNames: String*) = IncompleteSelect(columnNames)
  def col(columnName: String) = Column(columnName)

  val query =
    select("members.*", "someTable.column")
      .from("members")
      .leftJoin(
        "someTable",
        col("id").eq(col("someTable.id"))
          .and(col("someTable.id").notEq(3))
      )
      .innerJoin("anotherTable", col("id").eq(col("anotherTable.id")))
      .where(
        col("id").eq(2)
          .and(
            col("name").like("Bob%")
          )
      )
      .groupBy(col("name").asc
        .and(
          col("id").desc
        ))
}
