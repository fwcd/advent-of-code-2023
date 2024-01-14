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

@ExperimentalForeignApi
fun main(args: Array<String>) {
  if (args.isEmpty()) {
    println("Usage: day20 <path to input>")
    exitProcess(1)
  }

  val lines = readLines(args[0])
  println(lines)
}
