/**
 * http://stackoverflow.com/a/1076533
 */
object Int {

    def unapply(s : String) : Option[Int] = try {
        Some(s.toInt)
    } catch {
        case _ : java.lang.NumberFormatException => None
    }

}
