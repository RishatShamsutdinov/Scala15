import java.util.Random
import scala.collection.mutable.ArrayBuffer

/**
 * Created with IntelliJ IDEA.
 * User: MrDarK_AngeL
 * Date: 10.07.13
 * Time: 19:43
 * To change this template use File | Settings | File Templates.
 */
class Field {

    private val squadSize: Int = 4

    if (squadSize < 2) {
        throw new IllegalArgumentException("Illegal size of squad")
    }

    val format = "%2s "

    private val nums: Array[Int] = {
        val rand: Random = new Random
        var result: Array[Int] = null

        do {
            val freeNums: ArrayBuffer[Int] = ArrayBuffer(0 until squadSize * squadSize: _*)

            result = Array.fill(freeNums.length) {
                freeNums remove rand.nextInt(freeNums.length)
            }
        } while(!isSolvable(result))

        result
    }
    private var freePosition: Int = getFreePosition(nums)

    private def getFreePosition(nums: Array[Int]): Int = nums indexOf 0

    private def isSolvable(nums: Array[Int]): Boolean = {
        val e: Int = getPositionLine(getFreePosition(nums)) + 1

        ((nums.zipWithIndex map { case(x, idx) =>
            nums drop(idx + 1) count { y =>
                y != 0 && y < x
            }
        }).sum + e) % 2 == 0
    }

    private def getPositionLine(pos: Int): Int = pos / squadSize

    private def getPositionInSameLineWithFreePosition(pos: Int): Int = {
        val firstInLine = getPositionLine(freePosition) * squadSize

        math min(math max(firstInLine, pos), firstInLine + squadSize - 1)
    }

    def move(num: Int) {
        val idx: Int = getAndValidateMovingNumIdx(num)

        nums(idx) = nums(freePosition)
        nums(freePosition) = num

        freePosition = idx
    }


    def getAndValidateMovingNumIdx(num: Int): Int = {
        val idx: Int = nums indexOf num

        if (idx < 0) {
            throw new ArrayIndexOutOfBoundsException
        }

        if (!Array(
            getPositionInSameLineWithFreePosition(freePosition - 1),
            getPositionInSameLineWithFreePosition(freePosition + 1),
            freePosition - squadSize, freePosition + squadSize
        ).contains(idx)) {
            throw new IllegalFieldNumException
        }

        idx
    }

    private def removeFreeNumIfNeed(buf: ArrayBuffer[Int]) {
        for (pos <- Array(buf.length - 1, 0)) {
            if (buf(pos) == 0) {
                buf.remove(pos)
            }
        }
    }

    def isSorted: Boolean = {
        val numsToCheck: ArrayBuffer[Int] = ArrayBuffer(nums: _*)

        removeFreeNumIfNeed(numsToCheck)

        var result = true

        numsToCheck sliding 2 foreach { x =>
            result &= x(0) < x(1)
        }

        result
    }

    override def toString: String = {
        val sb: StringBuilder = new StringBuilder

        nums.zipWithIndex foreach { case(n, idx) =>
            if (idx > 0 && idx % squadSize == 0) {
                sb append '\n'
            }

            sb append (format format (n match {
                case 0 => "*"
                case _ => n.toString
            }))
        }

        sb.toString()
    }
}
