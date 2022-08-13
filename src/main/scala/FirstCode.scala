import javassist._
import org.opalj.br._
import org.opalj.br.analyses.Project

import java.io.FileInputStream
import java.net.URL


object FirstCode {

  val projectJAR = "C:/Users/tam20/java_bytecode_manipulation/project/project/target/lib/cinemark-dex2jar.jar"
  val p: Project[URL] = Project(new java.io.File(projectJAR))
  val reflectMethods: Array[String] = Array(
    "getDeclaredConstructor",
    "getConstructors",
    "getConstructors",
    "getDeclaredMethod",
    "getDeclaredMethods",
    "getMethods",
    "getMethod",
    "getClass",
    "getSuperclass",
    "forName",
    "getField",
    "getFields",
    "getDeclaredFields",
    "getDeclaredField",
    "getName",
    "getSimpleName",
    "isAssignableFrom",
    "invoke",
    "getClassLoader",
    "desiredAssertionStatus",
    "getCanonicalName",
    "get")

  def checkMethodUsingReflection(method: Method) : Boolean = {
    if (method.body.isDefined) {
      val code = method.body.get
      val i_invoke = code.instructions.filter(instr => instr != null && instr.isInvocationInstruction)
      val result = i_invoke.filter(instr => {
        val name = instr.asInvocationInstruction.name
        for (method_name <- reflectMethods){
          if (method_name == name)
            return true
        }
        return false
      })
      !result.isEmpty
    } else
      false
  }


  def main(args: Array[String]): Unit = {
    val methods_with_body = p.allMethodsWithBody
    val method_using_reflection = methods_with_body.filter(method => checkMethodUsingReflection(method))
    var count = 0;
    val pool = ClassPool.getDefault
    pool.insertClassPath(projectJAR)
    method_using_reflection.foreach(method => {
      val qualified_name = method.classFile.fqn.replace('/', '.')
      val cc = pool.get(qualified_name)
      try {
        var ct_method = cc.getMethod(method.name, method.descriptor.toJVMDescriptor)
        count += 1
        ct_method.setModifiers(Modifier.STATIC)
      } catch{
        case e: NotFoundException =>
      }
    })
    println(count);
  }
}
