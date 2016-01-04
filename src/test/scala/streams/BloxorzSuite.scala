package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) {
        case (block, move) => move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait Level0 extends SolutionChecker {
    /* terrain for level 0*/

    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin

    //val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 0") {
    new Level0 {
      assert(!terrain(Pos(0, 0)), "0,0")
      assert(!terrain(Pos(0, 0)), "0,5")
      assert(!terrain(Pos(4, 0)), "4,0")
      assert(!terrain(Pos(4, 0)), "4,5")
      assert(!terrain(Pos(0, 2)), "0,2")
      assert(!terrain(Pos(0, 3)), "0,3")
      assert(terrain(Pos(1, 2)), "1,2")
      assert(terrain(Pos(1, 3)), "1,3")
      assert(terrain(Pos(2, 2)), "2,2")
      assert(terrain(Pos(2, 3)), "2,3")
      assert(terrain(Pos(3, 2)), "3,2")
      assert(terrain(Pos(3, 3)), "3,3")
      assert(!terrain(Pos(4, 2)), "4,2")
      assert(!terrain(Pos(4, 3)), "4,3")
    }
  }

  test("findChar level 0") {
    new Level0 {
      assert(startPos == Pos(1, 2))
      assert(goal == Pos(1, 3))
    }
  }

  test("neighbors for level 0 - start") {
    new Level0 {
      val start = startBlock
      assert(startPos === start.b1)
      assert(startPos === start.b2)
      assert(start.isStanding)
      println(start.neighbors)
      println(start.legalNeighbors)
      assert(start.legalNeighbors.size === 1)
      assert(start.legalNeighbors.contains(Block(Pos(2, 2), Pos(3, 2)), Down))
    }
  }

  test("neighbors for level 0 - 2nd move") {
    new Level0 {
      val b = Block(Pos(2, 3), Pos(3, 3))
      assert(b.isStanding === false)
      println(b.neighbors)
      println(b.legalNeighbors)
      assert(b.legalNeighbors.size === 2)
      assert(b.legalNeighbors.contains(Block(Pos(2, 2), Pos(3, 2)), Left))
      assert(b.legalNeighbors.contains(Block(Pos(1, 3), Pos(1, 3)), Up))
    }
  }

  test("neighbors with history") {
    new Level0 {
      println("LEVEL0 NBH[(2,3),(3,2)]: " + neighborsWithHistory(Block(Pos(2, 2), Pos(3, 2)), List(Down)).toList)
    }
  }

  test("new neighbors only") {
    new Level0 {
      val neighbors = neighborsWithHistory(Block(Pos(2, 2), Pos(3, 2)), List(Down))
      val explored = Set(Block(Pos(1, 2), Pos(1, 2)))
      println("LEVEL0 NNO[(2,3),(3,2)]: " + newNeighborsOnly(neighbors, explored).toList)
    }
  }

  test("neighbors with history - next") {
    new Level0 {
      println("LEVEL0 NBH[(2,3),(3,3)]: " + neighborsWithHistory(Block(Pos(2, 3), Pos(3, 3)), List(Right, Down)).toList)
    }
  }

  test("new neighbors with only - next") {
    new Level0 {
      val neighbors = neighborsWithHistory(Block(Pos(2, 3), Pos(3, 3)), List(Right, Down))
      val explored = Set(Block(Pos(1, 2), Pos(1, 2)), Block(Pos(2, 2), Pos(3, 2)))
      println("LEVEL0 NNO[(2,3),(3,3)]: " + newNeighborsOnly(neighbors, explored).toList)
    }
  }

  test("neighbors with history - last") {
    new Level0 {
      println("LEVEL0 NBH[(1,3),(1,3)]: " + neighborsWithHistory(Block(Pos(1, 3), Pos(1, 3)), List(Up, Right, Down)).toList)
    }
  }

  test("new neighbors with only - last") {
    new Level0 {
      val neighbors = neighborsWithHistory(Block(Pos(1, 3), Pos(1, 3)), List(Up, Right, Down))
      val explored = Set(Block(Pos(1, 2), Pos(1, 2)), Block(Pos(2, 2), Pos(3, 2)), Block(Pos(2, 3), Pos(3, 3)))
      println("LEVEL0 NNO[(1,3),(1,3)]: " + newNeighborsOnly(neighbors, explored).toList)
    }
  }

  test("from 2nd position") {
    new Level0 {
      val initial = Set((Block(Pos(2, 2), Pos(3, 2)), List(Down))).toStream
      val explored = Set(startBlock)
      println("LEVEL0 FROM: " + from(initial, explored).toList)
    }
  }

  test("paths from start") {
    new Level0 {
      println("PFS: " + pathsFromStart.toList)
    }
  }

  test("paths to goal") {
    new Level0 {
      println("PTG: " + pathsToGoal.toList)
    }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(!terrain(Pos(-1, -1)), "-1,-1")
      assert(terrain(Pos(0, 0)), "0,0")
      assert(terrain(Pos(0, 2)), "0,2")
      assert(!terrain(Pos(0, 3)), "0,3")
      assert(terrain(Pos(1, 1)), "1,1") // S
      assert(terrain(Pos(4, 7)), "4,7") // T
      assert(terrain(Pos(4, 8)), "4,8")
      assert(terrain(Pos(4, 9)), "4,9")
      assert(!terrain(Pos(4, 11)), "4,11")
      assert(terrain(Pos(5, 8)), "5,8")
      assert(!terrain(Pos(5, 9)), "5,9")
      assert(!terrain(Pos(5, 15)), "5,15")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1, 1))
      assert(goal == Pos(4, 7))
    }
  }

  test("neighbors for level 1 - start") {
    new Level1 {
      val start = startBlock
      assert(start.b1 === startPos)
      assert(start.b2 === startPos)
      assert(start.b1 === Pos(1, 1))
      assert(start.b2 === Pos(1, 1))
      assert(start.isStanding)
      println(start.neighbors)
      println(start.legalNeighbors)
      //assert(start.legalNeighbors.size === 1)
      //assert(start.legalNeighbors.contains(Block(Pos(2, 2), Pos(3, 2)), Down))
    }
  }

  test("neighbors with history for level 1") {
    new Level1 {
      val n = neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).toSet
      assert(n === Set((Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)), (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))))
    }
  }

  test("new neighbors only for level 1") {
    new Level1 {
      val n = newNeighborsOnly(Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream,
        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1))))
      assert(n.toSet === Set(
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))))
      assert(n === Set(
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream)
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
  

}
