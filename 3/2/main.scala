import java.awt.Rectangle

import scala.io.Source

object Application {
	def main(args: Array[String]): Unit = {
		val format = raw"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)".r()
		val claims: Map[Int, Rectangle] = Source.fromFile("input").getLines
			.map { case format(id, x, y, dx, dy) => (id.toInt, new Rectangle(x.toInt, y.toInt, dx.toInt, dy.toInt)) }
    		.toMap
		val claimIDs = claims.keySet
		val intersectingClaimIDs = for (
			(id1, rect1) <- claims;
			(id2, rect2) <- claims;
			id <- List(id1, id2) if (id1 > id2) && (rect1 intersects rect2))
			yield id

		val out = (claimIDs -- intersectingClaimIDs).head

		println(out)
	}
}