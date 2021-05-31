package blur

object HorizontalBoxBlur extends HorizontalBoxBlurInterface {

  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for {
      y <- from until end
      x <- 0 until src.width
    } dst.update(x, y, boxBlurKernel(src, x, y, radius))
  }

  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val parRange = 0 to src.height by  ( src.height / math.min( numTasks, src.height ) )
    val pairedStripes = parRange.zip(parRange.tail)
    val taskList = pairedStripes.map{ case (from, to) => task(blur(src, dst, from ,to, radius)) }
    taskList.foreach(_.join())
  }

}
