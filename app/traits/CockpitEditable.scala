package traits

import models.cockpit._
import scala.concurrent.{ ExecutionContext, Future }
import reactivemongo.core.commands._
import ExecutionContext.Implicits.global
import controllers.cockpit.ListController.{ SelectedFilters, ListOptions }
import play.api.libs.json.{ JsObject, Json }
import helper.JsonHelper._
import scala.Some
import scala.Some
import reactivemongo.core.commands.Match
import scala.Some
import reactivemongo.core.commands.Limit
import reactivemongo.core.commands.Skip
import helper.MongoCollections._
import reactivemongo.core.commands.Match
import scala.Some
import reactivemongo.core.commands.Limit
import reactivemongo.core.commands.Skip
import helper.IdHelper
import play.modules.reactivemongo.json.BSONFormats._
import play.api.Logger

/**
 * User: Björn Reimer
 * Date: 3/11/14
 * Time: 2:01 PM
 */

case class CockpitEditableDefinition(name: String,
                                     getList: ListOptions => Future[CockpitList],
                                     delete: (String) => Future[LastError],
                                     create: CockpitListElement,
                                     getAttributes: String => Future[Option[Seq[JsObject]]])

trait CockpitEditable[A] extends Model[A] {

  def cockpitMapping: Seq[CockpitAttribute]

  def cockpitListFilters: Seq[CockpitListFilter]

  /**
   * Helper
   */
  def getTitles: Seq[String] = cockpitMapping.filter(_.getShowInList).map { _.getDisplayName }

  def getCockpitListElement(obj: A): CockpitListElement = {
    val js = Json.toJson(obj).as[JsObject]
    val id = (js \ "_id" \ "mongoId").as[String]
    val attributes: Map[String, Option[String]] = cockpitMapping.zipWithIndex.map {
      case (atr, index) =>
        (index.toString, atr.getListString(js))
    }.toMap
    new CockpitListElement(id, attributes)
  }

  def getCockpitList(listOptions: ListOptions): Future[CockpitList] = {
    val filterJsons = listOptions.filter.map {
      case SelectedFilters(filterName, term) =>
        // get filter from list
        cockpitListFilters.find(_.filterName.equals(filterName)) match {
          case None            => Json.obj()
          case Some(filterDef) => filterDef.filterFunction(term)
        }
    }
    // convert them to Mongo Match
    val matches = filterJsons.map { js => Match(toBson(js).get) }

    // add limit and offset
    val pipeline: Seq[PipelineOperator] = matches ++
      Seq(
        Skip(listOptions.offset),
        Limit(listOptions.limit))

    val aggregationCommand = Aggregate(col.name, pipeline)

    mongoDB.command(aggregationCommand).map {
      res =>
        {
          val list = res.toSeq.map { bson =>
            Json.toJson(bson).as[A]
          }
          val elements = list.map(getCockpitListElement)
          new CockpitList(getTitles, elements, cockpitListFilters)
        }
    }
  }

  def getAttributes(id: String): Future[Option[Seq[JsObject]]] =
    find(id).map {
      _.map { obj =>
        val js = Json.toJson(obj).as[JsObject]
        cockpitMapping.map(_.getEditJson(js)).filter(_.isDefined).map(_.get)
      }
    }

  def newCockpitListElement: CockpitListElement = {
    val obj = createDefault()
    col.insert(obj)
    getCockpitListElement(obj)
  }
}
