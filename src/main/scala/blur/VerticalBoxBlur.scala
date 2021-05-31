package blur

object VerticalBoxBlur extends VerticalBoxBlurInterface {

  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for {
      x <- from until end
      y <- 0 until src.height
    } dst.update(x, y, boxBlurKernel(src, x, y, radius))
  }

  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val parRange = 0 to src.width by ( src.width / math.min( numTasks, src.width ) )
    val pairedStripes = parRange.zip(parRange.tail)
    val taskList = pairedStripes.map{ case (from, to) => task(blur(src, dst, from ,to, radius)) }
    taskList.foreach(_.join())
  }

}
