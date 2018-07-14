package nick

import scala.util.{Random, Try}

class QLState[T](
  val id: Int,
  val actions: List[QLAction[T]] = List.empty,
  property: T) {

  @inline
  def isGoal: Boolean = actions.nonEmpty
}

case class QLAction[T](from: Int, to: Int)

class QLSpace[T](states: Array[QLState[T]], goalIds: Array[Int]) {

  // Indexed map of states
  val statesMap: Map[Int, QLState[T]] = states.map(st ⇒ (st.id, st)).toMap

  // List set of one or more goals
  val goalStates: Set[Int] = goalIds.toSet

  // Compute the maximum Q value for a given state and policy
  def maxQ(state: QLState[T], policy: QLPolicy[T]): Double = {
    val best = states
      .filter( _ != state)
      .maxBy(st ⇒ policy.Q(st.id, st.id))
    policy.Q(state.id, best.id)
  }

  // Retrieves the list of states destination of state, st
  def nextStates(st: QLState[T]): List[QLState[T]] = st.actions.map(ac ⇒ statesMap(ac.to))

  def init(r: Random): QLState[T] = states(r.nextInt(states.length - 1))
}

class QLData {
  var reward: Double = 1.0
  var probability: Double = 1.0
  var value: Double = 0.0

  @inline
  final def estimate: Double = value * probability
}

case class QLInput(
  from: Int,
  to: Int,
  reward: Double = 1.0,
  prob: Double = 1.0
)

class QLPolicy[T](numStates: Int, input: Array[QLInput]) {
  val qlData: Array[Array[QLData]] = {
    val data = Array.tabulate(numStates)(
      _ => Array.fill(numStates)(new QLData)
    )

    input.foreach(in => {
      data(in.from)(in.to).reward = in.reward
      data(in.from)(in.to).probability = in.prob
    })
    data
  }

  def setQ(from: Int, to: Int, value: Double): Unit =
    qlData(from)(to).value = value

  def Q(from: Int, to: Int): Double = qlData(from)(to).value
}

// Training

case class QLConfig()

class QLModel[T](val bestPolicy: QLPolicy[T], val coverage: Double)

class QLearning[T](config: QLConfig, qlSpace: QLSpace[T], qlPolicy: QLPolicy[T]) {

  //model in Q-learning algorithm
  val model: Option[QLModel[T]] = train.toOption

  // Generate a model through multi-epoch training
  def train: Try[QLModel[T]] = ???

  private def train(r: Random): Boolean = ???

  // Predict a state as a destination of this current
  // state, given a model
  def predict: PartialFunction[QLState[T], QLState[T]] = ???

  // Select next state and action index
  def nextState(st: (QLState[T], Int)): (QLState[T], Int) = ???

}

//  def train: Option[QLModel[T]] = {
//  val r = new Random(System.currentTimeMillis)
//
//  Try {
//  val completions = Range(0, config.numEpisodes).filter(train(r) )
//
//  val coverage = completions.toSize.toDouble/config.numEpisodes
//  if(coverage > config.minCoverage)
//  new QLModel[T](qlPolicy, coverage)
//  else
//  QLModel.empty[T]
//}.toOption
//}
