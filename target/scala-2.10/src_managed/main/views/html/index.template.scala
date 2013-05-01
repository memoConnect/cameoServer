
package views.html

import play.templates._
import play.templates.TemplateMagic._

import play.api.templates._
import play.api.templates.PlayMagic._
import models._
import controllers._
import play.api.i18n._
import play.api.mvc._
import play.api.data._
import views.html._
/**/
object index extends BaseScalaTemplate[play.api.templates.Html,Format[play.api.templates.Html]](play.api.templates.HtmlFormat) with play.api.templates.Template1[String,play.api.templates.Html] {

    /**/
    def apply/*1.2*/(message: String):play.api.templates.Html = {
        _display_ {

Seq[Any](format.raw/*1.19*/("""

"""),_display_(Seq[Any](/*3.2*/main("KolibriNet")/*3.20*/ {_display_(Seq[Any](format.raw/*3.22*/("""

    <h1>"""),_display_(Seq[Any](/*5.10*/(message))),format.raw/*5.19*/("""</h1>
""")))})),format.raw/*6.2*/("""
"""))}
    }
    
    def render(message:String): play.api.templates.Html = apply(message)
    
    def f:((String) => play.api.templates.Html) = (message) => apply(message)
    
    def ref: this.type = this

}
                /*
                    -- GENERATED --
                    DATE: Thu May 02 01:01:48 CEST 2013
                    SOURCE: /home/reimerei/dev/kolibrinet/app/views/index.scala.html
                    HASH: c76cfb426b87d5fffe79411760bb10f49669d21f
                    MATRIX: 505->1|599->18|636->21|662->39|701->41|747->52|777->61|814->68
                    LINES: 19->1|22->1|24->3|24->3|24->3|26->5|26->5|27->6
                    -- GENERATED --
                */
            