import scala.collection.mutable.LinkedHashMap
import scala.io.Source

/**
  * Parseur de logs pour des stats
  * Le programme peut etre executé de deux maniéres : Soit il prends comme entrée le path d'un fichier en local passé en argument,
  * ou, a defaut, lit sur l'entrée standard
  */
object StatLogParser {

  // point d'entrée
  def main(args: Array[String]): Unit = {

    // Si le path d'un fichier est passé en argument
    if (!args.isEmpty && args.length == 1) {

      // lire le fichier
      val fileSource = Source.fromFile(args(0));

      // Parcourir les lignes du fichier
      fileSource.getLines().foreach(line => parseLogLine(line));
      printTuple(current, tuple);
      fileSource.close();
    } else {

      // lire l'entrée standard
      Source.stdin.getLines().foreach(line => parseLogLine(line));
      printTuple(current, tuple);
    }
  }

  /** Lit et parse une ligne de log,calcule et met a jour les viewmodes successif,
    * leurs nombre d'occurence ainsi que leurs diifferentes valeurs de zoom
    */
  def parseLogLine(line: String): Unit = {
    val pattern = "^\\/map\\/1\\.0\\/\\w+\\/\\w+\\/[0-9]+\\/[0-9]+\\/[0-9]+\\/[0-9]+$";

      if (line.contains("/")) {
        val log = line.substring(line.indexOf("/"), line.length);
        val logArray = log.split("/");

        //define first viewMode
        if (current == "") {
          current = logArray(4);
          tuple = (1, logArray(6));
          return;
        }

        if (log.matches(pattern)) {
          if (pass || !logArray(4).equals(current)) {
            printTuple(current, tuple);
            current = logArray(4);
            tuple = (1, logArray(6));
          } else {
            tuple = (tuple._1 + 1, tuple._2 + "," + logArray(6));
          }

          pass = false;
        } else {
          // ignore non matching log
          pass = true;
        }
      } else {
        // ignore non log lines
        pass = true;
      }
  }

  /**
    * Affiche le resultat du parse sur la sortie standard
    */
  def printTuple(viewMode: String, result: Tuple2[Int, String]): Unit = {
    var zoomArray = result._2.split(",");
    var zoom = "";

    // eliminer les zoom en doublon
    zoomArray.foreach(z => {
      if (!zoom.contains(z)) {
        zoom = z + "," + zoom;
      }
    });

    if (zoom.endsWith(",")) {
      zoom = zoom.substring(0, zoom.length - 1);
    }

    // affichage du resultat
    println(viewMode + "\t" + result._1 + "\t" + zoom);
  }

  // Data Members
  var pass = false;     // flag pour log non conforme
  var current = "";     // viewMode en cours // successif
  var tuple = (0, "");  // tuple de resultat en cours
}
