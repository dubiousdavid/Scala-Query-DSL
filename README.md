# Scala Query DSL

```scala
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
```

Run the following in a REPL:

```scala
import QueryDSL._
val(sql, placeHolders) = query.toStatement
```
