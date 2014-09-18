package controllers.cockpit

import helper.AuthenticationActions._
import helper.ResultHelper._
import models.{ Account, CockpitAccess, Identity, MongoId }
import play.api.libs.json.{ Json, Reads }
import play.api.mvc.Result
import traits.{ CockpitEditableDefinition, ExtendedController }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * User: Björn Reimer
 * Date: 3/11/14
 * Time: 4:33 PM
 */
object ListController extends ExtendedController {

  // todo: find a better way to do this
  def allEditables = Seq(
    new CockpitEditableDefinition("identity", Identity.getCockpitList, Identity.delete, Identity.newCockpitListElement, Identity.getAttributes, Identity.updateElement),
    new CockpitEditableDefinition("account", Account.getCockpitList, Account.delete, Account.newCockpitListElement, Account.getAttributes, Account.updateElement)
  )

  def getEditable(name: String): Option[CockpitEditableDefinition] = {
    allEditables.find(definition => definition.name.equals(name))
  }

  case class SelectedFilters(name: String, term: String)

  object SelectedFilters {
    implicit val reads: Reads[SelectedFilters] = Json.reads[SelectedFilters]
  }

  case class ListOptions(limit: Int,
                         offset: Int,
                         filter: Seq[SelectedFilters])
  object ListOptions {
    implicit val reads: Reads[ListOptions] = Json.reads[ListOptions]
  }

  def getAllLists() = TwoFactorAuthAction().async {
    request =>
      checkAccessList(request.identity.accountId) {
        val allNames: Seq[String] = allEditables.map(_.name)
        Future(resOk(Json.obj("lists" -> Json.toJson(allNames))))
      }
  }

  def checkAccessList(accountId: Option[MongoId])(action: Future[Result]): Future[Result] = {
    accountId match {
      case None => Future(resUnauthorized("no account"))
      case Some(id) =>
        CockpitAccess.findByAccountId(id).flatMap {
          case None    => Future(resUnauthorized("not on access list"))
          case Some(a) => action
        }
    }
  }

  def list(elementName: String) = TwoFactorAuthAction().async(parse.tolerantJson) {
    request =>
      checkAccessList(request.identity.accountId) {
        validateFuture(request.body, ListOptions.reads) {
          listOptions =>
            {
              getEditable(elementName) match {
                case None => Future(resNotFound("elementName"))
                case Some(definition) => definition.getList(listOptions).map {
                  list =>
                    resOk(list.toJson)
                }
              }
            }
        }
      }
  }

  def delete(elementName: String, id: String) = TwoFactorAuthAction().async { request =>
    checkAccessList(request.identity.accountId) {
      getEditable(elementName) match {
        case None => Future(resNotFound("elementName"))
        case Some(obj) => obj.delete(id).map {
          _.ok match {
            case false => resServerError("could not delete")
            case true  => resOk("deleted")
          }
        }
      }
    }
  }

  def create(elementName: String) = TwoFactorAuthAction().async { request =>
    checkAccessList(request.identity.accountId) {
      getEditable(elementName) match {
        case None      => Future(resNotFound("elementName"))
        case Some(obj) => Future(resOk(obj.create.toJson))
      }
    }
  }
}

