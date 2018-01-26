package gov.nasa.jpl.imce.oml.converters

import java.io.File
import java.lang.System

import org.apache.spark.SparkConf
import org.apache.spark.sql.{Dataset, SparkSession}

import scala.{Array, Boolean, Int, StringContext, Unit}
import scala.Predef._

/**
  * Adapted dongjoon's example to mimick the OML Converter's parquet operations.
  * See https://issues.apache.org/jira/browse/SPARK-21349?focusedCommentId=16146569&page=com.atlassian.jira.plugin.system.issuetabpanels%3Acomment-tabpanel#comment-16146569
  */
object LargeTaskSizeDuringSave2Parquet {

  def main(argv: Array[String]): Unit = {
    val dir = new File("/tmp/spark-events")
    if (!dir.exists()) {
      dir.mkdir()
    }

    val conf = new SparkConf()
      .setMaster("local")
      .setAppName("Test")
      .set("spark.eventLog.enabled", "true")

    implicit val spark = SparkSession
      .builder()
      .config(conf)
      .getOrCreate()

    implicit val sqlContext = spark.sqlContext

    import spark.implicits._

    System.out.println(spark.version)

    val data: scala.collection.Seq[(Int, String, Boolean)] = (1 to (100 * 365 * 3)).map(i => (i, s"$i", i % 2 == 0))
    val ds: Dataset[(Int, String, Boolean)] = sqlContext.createDataset[(Int, String, Boolean)](data)
    ds.write.format("parquet").mode("overwrite").saveAsTable("t")

  }

}
