import kotlin.system.exitProcess
import kotlinx.cinterop.*
import platform.posix.*

// https://www.nequalsonelifestyle.com/2020/11/16/kotlin-native-file-io/
@ExperimentalForeignApi
fun readLines(filePath: String): List<String> {
  val lines = mutableListOf<String>()
  val file = fopen(filePath, "r") ?: throw IllegalArgumentException("Cannot open $filePath")
  try {
    memScoped {
      val bufferLength = 64 * 1024
      val buffer = allocArray<ByteVar>(bufferLength)
      var line = fgets(buffer, bufferLength, file)?.toKString()
      while (line != null) {
        lines.add(line)
        line = fgets(buffer, bufferLength, file)?.toKString()
      }
    }
  } finally {
    fclose(file)
  }
  return lines
}

private val NODE_PATTERN = """([%&]?)(\w+)\s+->\s+((?:\w+,\s*)*\w+)""".toRegex()

enum class NodeType {
  BROADCAST, FLIPFLOP, CONJUNCTION;

  companion object {
    fun parse(raw: String) = when (raw) {
      "&" -> NodeType.CONJUNCTION
      "%" -> NodeType.FLIPFLOP
      else -> NodeType.BROADCAST
    }
  }
}

data class Node<T>(
  val type: NodeType,
  val name: T,
  val outputs: List<T>
) {
  companion object {
    fun parse(raw: String): Node<String>? = NODE_PATTERN.matchEntire(raw.trim())?.let { match ->
      Node(
        type = NodeType.parse(match.groupValues[1]),
        name = match.groupValues[2],
        outputs = match.groupValues[3].split(",").map { it.trim() }
      )
    }
  }
}

value class BitArray(val value: ULong) {
  operator fun get(i: Int): ULong = (value shr i) and 1UL

  fun zero(i: Int) = BitArray(value and (1UL shl i).inv())

  fun flip(i: Int) = BitArray(value xor (1UL shl i))

  fun set(i: Int) = BitArray(value or (1UL shl i))

  fun set(i: Int, bit: ULong) = BitArray(zero(i).value or (bit shl i))

  override fun toString() = value.toString(radix = 2).padStart(64, '0')
}

data class Circuit(
  val nodes: List<Node<Int>>,
  val inputs: List<List<Int>>,
  val indexing: Map<String, Int>,
) {
  companion object {
    fun parse(lines: List<String>): Circuit {
      val strNodes = lines.mapNotNull { Node.parse(it) }
      val indexing = strNodes.mapIndexed { i, n -> Pair(n.name, i) }.toMap()
      val intNodes = strNodes.mapIndexed { i, n -> Node(n.type, i, n.outputs.map { indexing[it] ?: -1 }) }
      return Circuit(
        nodes = intNodes,
        inputs = intNodes.map { n -> intNodes.filter { n.name in it.outputs }.map { it.name } },
        indexing = indexing,
      )
    }
  }
}

data class Pulse(
  val receiver: Int,
  val value: ULong,
)

data class Runner(
  var memory: BitArray = BitArray(0UL),
  var lows: Int = 0,
  var highs: Int = 0,
) {
  fun run(circuit: Circuit, broadcaster: Int): BitArray {
    var pulsed = BitArray(0UL)
    var queue = ArrayDeque<Pulse>()
    queue.addLast(Pulse(broadcaster, 0UL))
    lows++
    while (queue.isNotEmpty()) {
      val pulse = queue.removeFirst()
      val i = pulse.receiver
      val node = circuit.nodes[i]
      var changed = false
      if (pulse.value == 0UL) {
        pulsed = pulsed.set(i)
      }
      when (node.type) {
        NodeType.FLIPFLOP -> {
          if (pulse.value == 0UL) {
            memory = memory.flip(i)
            changed = true
          }
        }
        NodeType.CONJUNCTION -> {
          memory = memory.set(i, 1UL - circuit.inputs[i].map { memory[it] }.reduce(ULong::and))
          changed = true
        }
        NodeType.BROADCAST -> {
          changed = true
        }
      }
      if (changed) {
        val bit = memory[i]
        if (bit == 1UL) {
          highs += node.outputs.size
        } else {
          lows += node.outputs.size
        }
        for (j in node.outputs) {
          if (j >= 0) {
            queue.addLast(Pulse(j, bit))
          }
        }
      }
    }
    return pulsed
  }
}

data class CycleTracker<T>(
  var last: T? = null,
  val cycles: MutableMap<T, ULong> = mutableMapOf(),
  var runningLength: ULong = 0UL,
) {
  val total: ULong
    get() = cycles.values.sum()

  fun feed(value: T) {
    if (value != last && runningLength > 0UL) {
      last?.let { cycles[it] = runningLength }
      runningLength = 0UL
    }
    runningLength++
    last = value
  }
}

@ExperimentalForeignApi
fun main(args: Array<String>) {
  if (args.isEmpty()) {
    println("Usage: day20 <path to input>")
    exitProcess(1)
  }

  val lines = readLines(args[0])
  val circuit = Circuit.parse(lines)
  val broadcaster = circuit.indexing["broadcaster"]!!

  run {
    val runner = Runner()
    repeat(1000) {
      runner.run(circuit, broadcaster)
    }
    println("Part 1: ${runner.lows * runner.highs}")
  }

  // The trick for part 2 is visualizing the input: There
  // are four binary counters and we need to find the LCM.
  // Since they have prime cycles, we can just multiply them.

  circuit.nodes.find { -1 in it.outputs }?.name?.let { rxInput ->
    val rxInputInputs = circuit.inputs[rxInput]
    val runner = Runner()
    val trackers = rxInputInputs.associateWith { CycleTracker<ULong>() }
    repeat (8000) { // Not hardcoding this would be nicer, but the condition seems to be nontrivial
      val pulsed = runner.run(circuit, broadcaster)
      for (i in trackers.keys) {
        trackers[i]!!.feed(pulsed[i])
      }
    }
    println("Part 2: ${trackers.map { it.value.total }.reduce(ULong::times)}")
  }
}
