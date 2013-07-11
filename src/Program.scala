import scala.util.control.Breaks._

/**
 * Created with IntelliJ IDEA.
 * User: MrDarK_AngeL
 * Date: 10.07.13
 * Time: 19:41
 * To change this template use File | Settings | File Templates.
 */
object Program extends App {

    val exitCmd = "exit"
    val field: Field = new Field

    var cmd = ""

    while(!cmd.equalsIgnoreCase(exitCmd)) {
        if (field.isSorted) {
            println("Win!")
            break
        }

        println(field)
        println()

        print("Number (or '%s' to exit): " format exitCmd)
        cmd = readLine()

        var mess: String = null

        try {
            cmd match {
                case Int(x) => field move x
                case _ => // do nothing
            }
        } catch {
            case _ : ArrayIndexOutOfBoundsException => mess = "Oops, out of range!"
            case _ : IllegalFieldNumException => mess = "'%s' is not available for moving" format cmd
            case e => throw e // something bad :(
        }

        if (mess != null) {
            println(mess)
        }

        println()
    }

}
