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

enum class NodeType {
  FLIPFLOP, CONJUNCTION
}

data class Node<T>(
  val type: NodeType,
  val name: String,
  val outputs: List<T>
)

private val NODE_PATTERN = """([%&]?)(\w+)\s+->\s+((?:\w+,\s*)*\w+)""".toRegex()

fun parseNodeType(raw: String) = when (raw) {
  "&" -> NodeType.CONJUNCTION
  else -> NodeType.FLIPFLOP
}

fun parseNode(raw: String) = NODE_PATTERN.matchEntire(raw.trim())?.let { match ->
  Node(
    type = parseNodeType(match.groupValues[1]),
    name = match.groupValues[2],
    outputs = match.groupValues[3].split(",").map { it.trim() }
  )
}

@ExperimentalForeignApi
fun main(args: Array<String>) {
  if (args.isEmpty()) {
    println("Usage: day20 <path to input>")
    exitProcess(1)
  }

  val nodes = readLines(args[0]).mapNotNull(::parseNode)
  println(nodes)
}
