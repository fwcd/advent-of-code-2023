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

value class Memory(
  val value: ULong
) {
  operator fun get(i: Int): ULong = (value shr i) and 1UL

  fun zero(i: Int) = Memory(value and (1UL shl i).inv())

  fun flip(i: Int) = Memory(value xor (1UL shl i))

  fun set(i: Int, bit: ULong) = Memory(zero(i).value or (1UL shl i))
}

data class Circuit(
  val nodes: List<Node<Int>>,
  val inputs: List<List<Int>>,
  val start: Int
) {
  companion object {
    fun parse(lines: List<String>, start: String = "broadcaster"): Circuit {
      val strNodes = lines.mapNotNull { Node.parse(it) }
      val indexing = strNodes.mapIndexed { i, n -> Pair(n.name, i) }.toMap()
      val intNodes = strNodes.mapIndexed { i, n -> Node(n.type, i, n.outputs.map { indexing[it]!! }) }
      return Circuit(
        nodes = intNodes,
        inputs = intNodes.map { n -> intNodes.filter { n.name in it.outputs }.map { it.name } },
        start = indexing[start]!!
      )
    }
  }

  fun run(memory: Memory): Memory {
    var memory = memory
    var queue = ArrayDeque<Int>()
    queue.addLast(start)
    while (queue.isNotEmpty()) {
      val i = queue.removeFirst()
      val node = nodes[i]
      when (node.type) {
        NodeType.FLIPFLOP -> {
          if (memory[i] == 0UL) {
            memory = memory.flip(i)
          }
        }
        NodeType.CONJUNCTION -> {
          memory = memory.set(i, inputs[i].map { memory[it] }.reduce(ULong::and))
        }
        else -> {}
      }
      for (j in node.outputs) {
        memory = memory.set(j, memory[i])
      }
    }
    return memory
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
  println(circuit)
}
