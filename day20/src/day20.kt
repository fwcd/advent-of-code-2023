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

value class Memory(val value: ULong) {
  operator fun get(i: Int): ULong = (value shr i) and 1UL

  fun zero(i: Int) = Memory(value and (1UL shl i).inv())

  fun flip(i: Int) = Memory(value xor (1UL shl i))

  fun set(i: Int, bit: ULong) = Memory(zero(i).value or (bit shl i))

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
  var memory: Memory = Memory(0UL),
  var lows: Int = 0,
  var highs: Int = 0,
  var received: Boolean = false,
) {
  fun run(circuit: Circuit, broadcaster: Int) {
    var queue = ArrayDeque<Pulse>()
    queue.addLast(Pulse(broadcaster, 0UL))
    lows++
    while (queue.isNotEmpty()) {
      val pulse = queue.removeFirst()
      val i = pulse.receiver
      val node = circuit.nodes[i]
      var changed = false
      when (node.type) {
        NodeType.FLIPFLOP -> {
          if (pulse.value == 0UL) {
            memory = memory.flip(i)
            changed = true
          }
        }
        NodeType.CONJUNCTION -> {
          val last = memory
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
          } else if (bit == 0UL) {
            received = true
          }
        }
      }
    }
  }
}

data class CycleTracker<T>(
  var expected: T,
  var cycleLength: Int = 0,
  var runningLength: Int = 0,
) {
  fun feed(value: T) {
    if (value == expected) {
      runningLength++
    } else if (runningLength > 0) {
      cycleLength = runningLength
      runningLength = 0
    }
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

  val runner = Runner()
  val trackers = List(circuit.nodes.size) { CycleTracker(1UL) }
  var i = 0
  while (!runner.received) {
    runner.run(circuit, broadcaster)
    i++
    for (i in 0 until circuit.nodes.size) {
      trackers[i].feed(runner.memory[i])
    }
    if (i % 1000 == 0) {
      println(listOf("qr", "kf", "kr", "zs").map { trackers[circuit.indexing[it]!!].cycleLength })
    }
  }
  println("Part 2: ${i}")
}
