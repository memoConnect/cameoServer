package traits

import models.cockpit._
import scala.concurrent.{ ExecutionContext, Future }
import reactivemongo.core.commands._
import ExecutionContext.Implicits.global
import controllers.cockpit.ListController.{SelectedFilters, ListOptions}
import play.api.libs.json.Json
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

/**
 * User: Björn Reimer
 * Date: 3/11/14
 * Time: 2:01 PM
 */

case class CockpitEditableDefinition(name: String,
                                     getList: ListOptions => Future[CockpitList],
                                     delete: (String) => Future[LastError],
                                     create: CockpitListElement, // ToDo change to edit element
                                     getEdit: (String, String) => Future[Option[CockpitElement]])

trait CockpitEditable[A] extends Model[A] {

  def cockpitListMapping(obj: A): (Seq[(String, Option[String])], String)

  def cockpitEditMapping(obj: A): Seq[CockpitAttribute]

  def cockpitListFilters: Seq[CockpitListFilter]

  /**
   * Helper
   */
  def getTitles(obj: A): Seq[String] = cockpitListMapping(obj)._1.map {
    case (key, value) => key
  }

  def toCockpitListElement(obj: A): CockpitListElement = {
    val (mapping, id) = cockpitListMapping(obj)
    val attributes: Map[String, Option[String]] = mapping.filter(_._2.isDefined).map {
      case (key, maybeValue) =>
        val index = getTitles(obj).indexWhere(_.equals(key)).toString
        (index, maybeValue)
    }.toMap
    new CockpitListElement(id, attributes)
  }

  def getList(listOptions: ListOptions): Future[CockpitList] = {
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
        val elements = list.map(toCockpitListElement)
        val titles = list.headOption.map(getTitles).getOrElse(Seq())
        new CockpitList(titles, elements, cockpitListFilters)
      }
    }
  }

  def getEdit(id: String, name:String): Future[Option[CockpitElement]] = find(id).map {
    maybeObj =>
      maybeObj.map {
        obj =>
          val attributes = cockpitEditMapping(obj)
          new CockpitElement(id, name, attributes)
      }
  }

  def createCockpitElement: CockpitListElement = {
    val identity = this.create(None, IdHelper.generateCameoId, None, None)
    col.insert(identity)
    toCockpitListElement(identity)
  }
}
