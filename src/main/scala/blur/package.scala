import java.util.concurrent._
import scala.util.DynamicVariable

import org.scalameter._

package object blur extends BoxBlurKernelInterface {
  type RGBA = Int

  def red(c: RGBA): Int = (0xff000000 & c) >>> 24

  def green(c: RGBA): Int = (0x00ff0000 & c) >>> 16

  def blue(c: RGBA): Int = (0x0000ff00 & c) >>> 8

  def alpha(c: RGBA): Int = (0x000000ff & c) >>> 0

  def rgba(r: Int, g: Int, b: Int, a: Int): RGBA = {
    (r << 24) | (g << 16) | (b << 8) | (a << 0)
  }

  def clamp(v: Int, min: Int, max: Int): Int = {
    if (v < min) min
    else if (v > max) max
    else v
  }

  class Img(val width: Int, val height: Int, private val data: Array[RGBA]) {
    def this(w: Int, h: Int) = this(w, h, new Array(w * h))
    def apply(x: Int, y: Int): RGBA = data(y * width + x)
    def update(x: Int, y: Int, c: RGBA): Unit = data(y * width + x) = c
  }

  def boxBlurKernel(src: Img, x: Int, y: Int, radius: Int): RGBA = {

    val xMin = clamp(x - radius, 0, src.width - 1)
    val xMax = clamp(x + radius, 0, src.width - 1)
    val yMin = clamp(y - radius, 0, src.height - 1)
    val yMax = clamp(y + radius, 0, src.height - 1)

    var r, g, b, a = 0

    var numPixels = 0

    var ypos = yMin

    while (ypos <= yMax) {
      var xpos = xMin
      while (xpos <= xMax) {
        val pixel = src(xpos, ypos)
        r += red(pixel)
        g += green(pixel)
        b += blue(pixel)
        a += alpha(pixel)
        numPixels += 1
        xpos += 1
      }
      ypos += 1
    }

    rgba(
      r / numPixels,
      g / numPixels,
      b / numPixels,
      a / numPixels
    )
  }

  val forkJoinPool = new ForkJoinPool

  abstract class TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T]
    def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
      val right = task {
        taskB
      }
      val left = taskA
      (left, right.join())
    }
  }

  class DefaultTaskScheduler extends TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T] = {
      val t = new RecursiveTask[T] {
        def compute = body
      }
      Thread.currentThread match {
        case wt: ForkJoinWorkerThread =>
          t.fork()
        case _ =>
          forkJoinPool.execute(t)
      }
      t
    }
  }

  val scheduler =
    new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)

  def task[T](body: => T): ForkJoinTask[T] = {
    scheduler.value.schedule(body)
  }

  def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
    scheduler.value.parallel(taskA, taskB)
  }

  def parallel[A, B, C, D](taskA: => A, taskB: => B, taskC: => C, taskD: => D): (A, B, C, D) = {
    val ta = task { taskA }
    val tb = task { taskB }
    val tc = task { taskC }
    val td = taskD
    (ta.join(), tb.join(), tc.join(), td)
  }

  implicit def keyValueCoerce[T](kv: (Key[T], T)): KeyValue = {
    kv.asInstanceOf[KeyValue]
  }
}
