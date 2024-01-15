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
  FLIPFLOP, CONJUNCTION;
  companion object {
    fun parse(raw: String) = when (raw) {
      "&" -> NodeType.CONJUNCTION
      else -> NodeType.FLIPFLOP
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

data class Circuit(
  val nodes: List<Node<Int>>,
  val start: Int
) {
  companion object {
    fun parse(lines: List<String>, start: String = "broadcaster"): Circuit {
      val strNodes = lines.mapNotNull { Node.parse(it) }
      val indexing = strNodes.mapIndexed { i, n -> Pair(n.name, i) }.toMap()
      val intNodes = strNodes.mapIndexed { i, n -> Node(n.type, i, n.outputs.map { indexing[it]!! }) }
      return Circuit(nodes = intNodes, start = indexing[start]!!)
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
  println(circuit)
}
